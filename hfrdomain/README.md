# Sample project for Domain Modeling using Haskell

In this project we explore the techniques of using domain driven design (DDD) using Haskell and pure functional programming. 

## Purpose

The main purpose of this project is _not_ to build a robust application using the principles of DDD. The idea is to explore the various capabilities of Haskell that apply in designing an application based on the principles of domain driven design. We discuss some of the principles of DDD that mesh well with Haskell as an implementation language.

### Communication and use of the ubiquitous language

Regarding the use of language, Eric Evans writes in the famous blue book:

> This is a crucial cord that weaves the model into development activity and binds it in the code.

Weaving the ubiquitous language into the code is best performed when you have an expressive language of implementation. If the implementation language is not so expressive you can choose to develop a DSL [1] - either an external one or an embedded one. With Haskell you don't need an additional linguistic layer for expressivity. Haskell is an expressive language - the only thing you need to take care of is to program at the proper level of abstraction.

### Clean separation of the life cycle of a domain object

Eric Evans notes aggregates, factories, repositories etc. as forming the lifecycle of a domain object. Haskell, as a language offers features to make these explicit abstractions. We can use

* algebraic data types (along with pattern matching) for modeling aggregates
* smart constructors can be used as factories to create _valid_ domain objects
* inductive data types coupled with effects can be used to model repositories and services

### Supple Design

This one is straight out of the blue book:

* **Intention Revealing Interfaces** - This means the ability to _name classes and operations to describe their effect and purpose, without reference to the means by which they do what they promise_. Haskell is a great platform for this, where we have a complete separation of the _how_ of a function from the _what_ - not only by name but also by the implementation.

* **Side-effect free Functions** - Haskell is pure (well, almost) and allows to model side-effects through algebraic effects. You can reason about your functions equationally.

* **Assertions** - Eric Evans says _Assertions make side effects explicit and easier to deal with_. With Haskell you have the benefit of a static type system to assert your invariants. And you can model side-effects algebraically - hence you don't need any additional external capability of assertions. They are just part of the language.

* **Conceptual Contours** - This is all about modularity. Haskell offers several ways to handle modularity of your application. Though not the strongest part of haskell, but you have constructs like typeclasses, modules etc. which can help you in modularizing your domain concepts.

* **Standalone Classes** - This is a technique that encourages building self-contained abstractions. Firstly Haskell encourages pure functions, which does not depend on external factors and only depends on the input that the functions accept. If the function / abstraction interacts with external systems, you use the type system to annotate them explicitly - so the contract is published and explicit. The type system enforces that you cannot fool around with the contracts of your domain behaviors.

* **Closure of Operations** - This is all about purity and compositionality. And Haskell shines with both of these. Just compose functions to define _what_ you want to do. Once you have built the abstraction of the complete domain behavior, you can run an interpreter that executes _how_ the behavior will be implemented. Here's an example of a domain service that runs a bunch of debits and credits on a bank account:


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
makeAccount :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Account
```

In this example `makeAccount` takes a date time (which is the current date) and a `Value` which is an aeson type (JSON) and generates an `Account` under an effectful context. The context is used for the validation of the aggregate. We will talk about the validation part very soon.

* A model comes out from the json value _always_ as a _validated_ entity - the aggregate. The validation is done using [`validation-selective`] (https://hackage.haskell.org/package/validation-selective) library. It offers validation semantics based on the applicative and selctive functors. The validation of model offers an accumulating semantics - all errors are accumulated and then reported to the client. 

* Model validation uses the _parse-then-validate_ idiom, loosely corresponding to what [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) recommends. We parse the json data and only if it passes the structural validation, we do the domain level validations. Here's an example of how we parse a date in json and then validate for the domain logic:

```haskell
parseAccountOpenDate :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) UTCTime
parseAccountOpenDate curr v = case validationToEither $ asDate v of
  Right dt -> validateAccountOpenDate curr dt
  Left  e  -> eitherToValidation (Left e)
      
validateAccountOpenDate :: UTCTime -> UTCTime -> Validation (NonEmpty ErrorInfo) UTCTime
validateAccountOpenDate current d = d <$ failureIf (current < d) (InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future"))
```

* Every model is its own module. It has the data and the functions that go along with the model. These functions are the domain behaviors for that model only.

* Currently we have 2 model elements - `Account` and `Transaction`, defined as follows:

```haskell
Account json  
    accountNo                   Text
    accountType                 AccountType sqltype=varchar  
    accountHolderName           Text  
    accountOpenDate             UTCTime default=CURRENT_TIME
    accountCloseDate            UTCTime Maybe default=NULL
    currentBalance              MoneyUSD
    rateOfInterest              Double 
    Primary accountNo
    deriving Show 

Transaction json
    transactionType             TransactionType sqltype=varchar
    transactionDate             UTCTime default=CURRENT_TIME
    transactionAmount           MoneyUSD
    transactionAccountNo        Text
    Foreign                     Account fkAccount transactionAccountNo
    deriving Show 
```

In `Account` we have `accountNo` as the primary key, which is also the foreign key in `Transaction` named `transactionAccountNo`. In `Transaction` we rely on an automatically generated `id` as the primary key. The domain models are linked to the database schema using `persistent` that generates the necessary artifacts  using Template Haskell.

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

### Effects for Testing

To run unit tests we may want to run the repository APIs without an actual database engine. In that case we can just supply a different set of effects using the State monad:

```haskell
-- | Instance of the interpreter that can be used for testing
type AccountMap = M.Map T.Text Account

runAccountRepositoryInMemory :: forall r a. Member (S.State AccountMap) r => Sem (AccountRepository ': r) a -> Sem r a
runAccountRepositoryInMemory = interpret $ \case
  QueryAccount accountID    -> S.gets (M.!? accountID)
  Store acc                 -> S.modify (M.insert (acc ^. accountNo) acc)
  StoreMany accs            -> S.modify (M.union (M.fromList ((\acc -> (acc ^. accountNo, acc)) <$> accs)))
  AllAccounts               -> S.gets M.elems
  QueryByOpenDate d         -> S.gets (M.elems . M.filter (\a -> a ^. accountOpenDate == d))
  Upsert acc                -> S.modify (M.insert (acc ^. accountNo) acc)
```

Of course one other alternative is to use the in-memory version of the database to run unit tests.

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

## An Optional Caching Layer

With polysemy implementing a caching layer as a separate effect is a breeze. This project has a sample implementation that demonstrates how to implement a domain service that uses the caching layer.

But first the implementation of the caching layer - we do it for `Account`:

**Step 1:** Define the contract of an `AccountCache`

```haskell
data AccountCache m a where
    CacheAccount         :: Account -> AccountCache m ()
    CacheAccounts        :: [Account] -> AccountCache m ()
    FetchCachedAccount   :: T.Text -> AccountCache m (Maybe Account)
    DeleteCachedAccount  :: T.Text -> AccountCache m ()
```

**Step 2:** Design a generic interpreter for running a cache layer under polysemy. We use `Redis` as the cache layer

```haskell
runCache :: forall b r. Members [Embed IO, Input Connection] r => Redis b -> Sem r b
runCache action = embed . flip runRedis action =<< input 
```

**Step 3:** Implement an interpreter for `AccountCache`

```haskell
runAccountCache :: forall r b. Members [Embed IO, Input Connection] r => Sem (AccountCache ': r) b -> Sem r b
runAccountCache = interpret $ \case
  CacheAccount acc -> runCache (void $ setex (pack . show $ acc ^. accountNo) 3600 (pack . show $ acc))
  CacheAccounts accs -> ...
  ...
```

### Implementing a cache enabled Domain Service

Let's implement a domain service that transfers funds across 2 accounts. We will use the `AccountRepository` for the database contracts and `AccountCache` for the cache layer.

```haskell
-- | Transfer amount from one account to another
-- | This is a cached service where we try to fetch accounts from the cache first and if that
-- | fails we fall back to the database. Similarly after update to database we update the cache
-- | with the updated accounts
transferCached :: Pool SqlBackend -> Connection -> T.Text -> T.Text -> Y.Dense "USD" -> IO ()
transferCached conn rconn fromAccountNo toAccountNo amount = 
  runAllEffectsWithCache conn rconn doTransfer
    where 
      doTransfer = updateAccountBalances >>= \accs -> do
        upsertMany accs
        cacheAccounts accs 
          where
            updateAccountBalances = 
              updateBalances <$> 
                    runMaybeT 
                          (MaybeT (fetchCachedAccount fromAccountNo) 
                      <|>  MaybeT (queryAccount fromAccountNo))
                <*> runMaybeT 
                          (MaybeT (fetchCachedAccount toAccountNo) 
                      <|>  MaybeT (queryAccount toAccountNo))
                where
                  updateBalances (Just fa) (Just ta)  = [fa & currentBalance %~ subtract amount, ta & currentBalance %~ (+ amount)]
                  updateBalances (Just _) Nothing     = error $ "To account [" ++ show toAccountNo ++ "] does not exist"
                  updateBalances Nothing (Just _)     = error $ "From account [" ++ show fromAccountNo ++ "] does not exist"
                  updateBalances Nothing Nothing      = error $ "From account [" ++ show fromAccountNo ++ "] and To account [" ++ show toAccountNo ++ "] does not exist"
```

Here are a few points regarding the above implementation:

* The query of the accounts follow a two staged fetch strategy - first we check the cache and then fall back to database if the account is not present in the cache
* Note the use of the `Alternative` typeclass combinator `<|>` to implement the 2 stage fetch
* `upsertMany` updates the database and `cacheAccounts` updates the cache
* The whole service runs within `runAllEffectsWithCache` which publishes the effects that we need to polysemy. Here's how we define it:

```haskell

runAllEffectsWithCache :: Pool SqlBackend -> Connection -> Sem '[AccountRepository, AccountCache, Input (Pool SqlBackend), Input Connection, Embed IO] a -> IO a
runAllEffectsWithCache conn rconn program =
  program                    
    & runAccountRepository
    & runAccountCache
    & runInputConst conn  
    & runInputConst rconn  
    & runM
```

## Integrating them all

Integrating the domain model, repository and services together is quite straightforward once you have the individual components aptly typed. Here's how to execute the above service API using a connection pool from a sqlite database engine:

```haskell
execute :: T.Text -> IO ()
execute accountno = runStdoutLoggingT
  . withSqlitePool connectionString openConnections
    $ \pool -> liftIO $ netValueTransactionsForAccount pool accountno
```
