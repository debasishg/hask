# Sample project for Domain Modeling using Haskell

In this project we explore the techniques of using domain driven design (DDD) using Haskell and pure functional programming. 

## The Main Artifacts

As is common with any DDD project, we have the following main artifacts:

* The **Model** capturing the main aggregates and entities
* The **Repository** that abstracts the data layer
* The **Domain Services** that offer coarser level abstractions for interacting with the client

In the current example, so far we have a very simple model - only 2 aggregates, `Account` and `Transaction`, which are related in a `1 .. m` relation.

For every model we have a corresponding repository that offers functions to operate on the repository in a manner which is independent of the underlying database engine.

Each model also has a service abstraction that offers ways to interact with the model and the repository. However the service layer can also operate on multiple repositories simulataneously in order to serve more complex business use cases.

## The Model

Every model in the project is designed using the following principles:

* A model is built as an aggregate using a smart constructor that takes an unvalidated object entering the bounded context from the edges. In this project we assume a json value entering the context from outside and getting the shape of a validated aggregate as it leaves the smart constructor. Here's an example for the `Account` model:

```haskell
makeAccount :: forall m. (MonadReader Env m, 
                          MonadValidate [Error] m => Value -> IO (m Account)
```

In this example `makeAccount` takes a `Value` which is an aeson type and generates an `Account` under a monadic context. The context is used for the validation of the aggregate. We will talk about the validation part very soon.

* A model comes out from the json value _always_ as a _validated_ entity - the aggregate. The validation is done using a monad transformer `ValidateT` offered by the [`monad-validate`](https://hackage.haskell.org/package/monad-validate-1.2.0.0/docs/Control-Monad-Validate.html) library. `ValidateT`, despite being a monad transformer, is designed not to necessarily halt on the first error. Hence the validation of model offers an accumulating semantics - all errors are accumulated and then reported to the client. Here's a snippet how we use the monadic validation and yet have the _accumulating semantics_ of all errors :

```haskell
makeAccount req = do 
    utcCurrent <- getCurrentTime  
    return $ withObject "request" req $ \o -> do

        accType         <- withKey o "account_type" parseAccountType
        accNo           <- withKey o "account_no" parseAccountNo
        accName         <- withKey o "account_name" parseAccountName
        accOpenDate     <- withKey o "account_open_date" (parseAccountOpenDate utcCurrent)
        accCloseDate    <- withKey o "account_close_date" (parseAccountCloseDate accOpenDate)
        currBalance     <- withKey o "account_current_balance" parseCurrentBalance
        rateOfInt       <- withKey o "rate_of_interest" (parseRateOfInterest accType)
        ...
```

* Model validation uses the _parse-then-validate_ idiom, loosely corresponding to what [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) recommends. We parse the json data and only if it passes the structural validation, we do the domain level validations. Here's an example of how we parse a date in json and then validate for the domain logic:

```haskell
parseAccountOpenDate curr v = do 
    dt       <- asDate v
    openDate <- tolerate $ validateAccountOpenDate curr dt
    pure $ fromJust openDate
      
validateAccountOpenDate :: UTCTime -> UTCTime -> m UTCTime
validateAccountOpenDate current d = 
    if current >= d 
    then pure d
    else refuteErr $ InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future")
```

* Every model is its own module. It has the data and the functions that go along with the model. These functions are the domain behaviors for that model only.

## The Repository

This is the abstraction over the data layer and contains the functions to access the data layer without worrying too much about the details of the underlying database platform. The repository is also a domain abstraction - hence the contracts that it publishes need to speak the domain vocabulary. Here's a sample of the contracts that the `AccountRepository` publishes:

```haskell
-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepository m a where
    QueryAccount         :: T.Text -> AccountRepository m (Maybe Account)
    Store                :: Account -> AccountRepository m ()
    StoreMany            :: [Account] -> AccountRepository m ()
    QueryByOpenDate      :: UTCTime -> AccountRepository m [Account]
    AllAccounts          :: AccountRepository m [Account]
    Upsert               :: Account -> AccountRepository m Account
``` 

> **Note:** In an alternative approach we thought of using a typeclass to model a Repository. It works but modeling as a simple record of functions is possibly one way of encouraging the principle _do-the-simplest-thing-that-works_. And I am always a bit uncomfortable with typeclasses that have no laws.

### Effectful Contracts

A repository at the implementation level interacts with external systems like databases. Hence it's imperative that this layer of abstraction be effectful. In this iteration of design we used an effects library [polysemy](https://hackage.haskell.org/package/polysemy). I am not going to discuss details of what polysemy does - there are quite a few blog posts on this. What makes use of polysemy (or other similar libraries) effective is that it makes the effects very explicit as part of the contract. Here's an example of how we interpret each of our repository's functions effectfully:

```haskell
runAccountRepository :: forall r b. Members [Embed IO, Input (Pool SqlBackend)] r => Sem (AccountRepository ': r) b -> Sem r b
```

In polysemy, the core abstraction is the `Sem` monad - In `Sem r a`, the type parameter `r` is a type-level list of effects. And we want `AccountRepository` to be a member of that list. 

Also the above signature makes it explicit that our repository implementation will have effects like `IO` and `(Pool SqlBackend)` which are made explicit as constraints on `r`. 

`Input` is a polysemy effect and we are using it to get a `(Pool SqlBackend)` as input to our functions - the pool provides the database connections which we will use for database access. But note these are part of the interpretation of the effect and _not_ the contract.

Similarly our output has to be `IO` since we are doing database I/O - hence we use the polysemy effect `Embed` to lift our `IO` into the polysemy effect stack.

Further note that we are using [`persistent`](https://hackage.haskell.org/package/persistent) as the basic abstraction for database layer.

### Interpreting Effects

The core design of an effectful repository is principled around an integration of `persistent` with `polysemy`. And this is the _final_ interpreter that will run each of the effectful functions of the repository. (Thanks to 
Georgi Lyubenov on Haskell mailing list for the help in designing this).

```haskell
runDB :: forall b r. Members [Embed IO, Input (Pool SqlBackend)] r => SqlPersistT IO b -> Sem r b
runDB action = embed . runSqlPool action =<< input
```

`runDB` deals with 2 effects:

* an input effect from which we receive our connection pool
* run the database queries in `IO` and embed the `IO` in the resulting effect stack

This design honors one of the salient principles of DDD - _making implicit things explicit_. Though Eric talks about this in the context of domain behaviors, using effects in this way also makes some aspects of your implementation very explicit.

With `runDB` as the interpreter, the rest of the interpretation of our repository functions become as simple as using the `Database.persist` APIs:

```haskell
runAccountRepository = interpret $ \case
  QueryAccount ano -> runDB (get (AccountKey ano)) 
  ... 
```

## The Service Layer

Ideally in a DDD setting we would like to think of repository as still a lower level abstraction. We would like to have coarser APIs that use a repository and deliver domain behaviors. All the implementation effects that we talked about in the last section should not surface in the end user API. 

Enter domain services.

Suppose I would like to query all accounts that were opened on a specific date. We need an API that takes a database connection, a specific date and would return a list of accounts as output. We are doing IO and hence it has also got to be part of the signature. Here's an example :


```haskell
queryAccountsByOpenDate :: Pool SqlBackend -> UTCTime -> IO [Account]
queryAccountsByOpenDate conn dt =
  runAllEffects conn (queryByOpenDate dt)
```

Here `queryByOpenDate` is a function of the repository which needs to be run under all the effects so that we ultimately land up only with an `IO`.

It's the functionality of `runAllEffects` that handles all effects, peels them all one by one and hands over the `IO` to us :

```haskell
runAllEffects :: Pool SqlBackend -> Sem '[AccountRepository, Input (Pool SqlBackend), Embed IO] a -> IO a
runAllEffects conn program =
  program                    -- [AccountRepository, Input (Pool SqlBackend), Embed IO] 
    & runAccountRepository   -- [Input (Pool SqlBackend), Embed IO]
    & runInputConst conn     -- [Embed IO]
    & runM
```

The various steps show the effects that are peeled off in successive interpretations of the effects stack.

### A more complex Domain Service

A domain service may be more complex and may need to interact with multiple repositories. Note that our repositories are effectful. Hence when we need to use multiple repositories within the boundary of a single service, those effecst need to compose. And this is one of the nicest features of polysemy - **effects compose vertically and horizontally**.

Here's an example:

```haskell
-- | a domain service that accesses multiple repositories to fetch account and
-- | transactions, computes the net value of all transactions fetched and updates
-- | the balance in the account repository
netValueTransactionsForAccount :: Pool SqlBackend -> Text -> IO ()
netValueTransactionsForAccount conn ano = runAllEffects conn doNetValueComputation
  where 
  ...
```

Te implementation of `netValueTransactionsForAccount` is not important as long as we can ensure that it can use all the effectful functions of the underlying repositories. The secret sauce is, once again, `runAllEffects` which has to ensure that effects are sequenced compositionally:

```haskell
runAllEffects :: 
     Pool SqlBackend 
  -> Sem '[AR.AccountRepository, TR.TransactionRepository, Input (Pool SqlBackend), Embed IO] a 
  -> IO a

runAllEffects conn program =
  program                          -- [AccountRepository, TransactionRepository, Input (Pool SqlBackend), Embed IO]  
    & AR.runAccountRepository      -- [TransactionRepository, Input (Pool SqlBackend), Embed IO]
    & TR.runTransactionRepository  -- [Input (Pool SqlBackend), Embed IO]
    & runInputConst conn           -- [Embed IO]
    & runM
```

Almost similar to what we had in the last section - just added another repository to the stack - polysemy takes care of the rest.

## Integrating them all

Integrating the domain model, repository and services together is quite straightforward once you have the individual components aptly typed. Here's how to execute the above service API using a connection pool from a sqlite database engine:

```haskell
execute :: T.Text -> IO ()
execute accountno = runStdoutLoggingT
  . withSqlitePool connectionString openConnections
    $ \pool -> liftIO $ netValueTransactionsForAccount pool accountno
```
