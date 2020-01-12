Question 1. Functions in mathematics and Haskell.
Suppose f : Int → Int sends an integer to its square, f(x) := x^2 and that g : Int → Int sends an integer to its successor, g(x) := x + 1.
(a) Write f and g in Haskell, including their type signature and their implementation.

> let f :: Int -> Int
>     f x = x ^ 2
>
> let g :: Int -> Int
>     g = (+1)

(b) Let h := f ◦ g. What is h(2)?

> let h :: Int -> Int
>     h = f . g
>
> h 2 -- 9

(c) Let i := f ; g. What is i(2)?

> let i :: Int -> Int
>     i = g . f
>
> i 2 -- 5

Question 2. Two small categories.

Recall that a category consists of the data:
  1. a set Ob(C) of objects;
  2. for every pair of objects c, d ∈ Ob(C) a set C(c, d) of morphisms;
  3. for every three objects b, c, d and morphisms f : b → c and g : c → d, a specified morphism (f ; g): b → d called the composite of f and g;
  4. for every object c, an identity morphism idc ∈ C(c, c); and subject to two laws:
  Unit: for any f : c → d, the equations idc ; f = f and f ; idc = f hold.
  Associative: for any f1 : c1 → c2, f2 : c2 → c3, and f3 : c3 → c4, the equation (f1 ; f2) ; f3 = f1 ; (f2 ; f3) holds.

  This tiny category is sometimes called the walking arrow category 2.

                           f
          2 :=   1 o-------------->o 2
		  /\               /\
		  id1              id2

(a) Write down the set of objects, the four sets of morphisms, the composition rule, and the identity morphisms.

1. Objects : {1, 2}
2. Morphisms: { id1 :: 1 -> 1, id2 :: 2 -> 2, f :: 1 -> 2 }
3. Composition:
   a. id1 :: 1 -> 1, f :: 1 -> 2 => id1 ; f = f (composite of id1 and f) 
   b. id2 :: 2 -> 2, f :: 1 -> 2 => f ; id2 = f (composite of f and id2) 
4. Identity morphisms: { id1 :: 1 -> 1, id2 :: 2 -> 2 }

(b) Prove that this category obeys the unit and associative laws.

1. Unit: for f :: 1 -> 2 and id1 :: 1 -> 1, id1 ; f = f (follow the types)
         for id2 :: 2 -> 2 and f :: 1 -> 2, f ; id2 = f (follow the types)

2. Associative: for id1 :: 1 -> 1, f :: 1 -> 2 and id2 :: 2 -> 2, we have the following:

   - (id1 ; f) ; id2 = (1 -> 2) ; (2 -> 2) = (1 -> 2) and
   - id1 ; (f ; id2) = (1 -> 1) ; (1 -> 2) = (1 -> 2) 

Question 3. Is it an isomorphism?

Suppose that someone tells you that their category C has two objects c, d and two non-identity morphisms, f : c → d and g : d → c, but no other morphisms. Does f have to be the inverse of g, i.e. is it forced by the category axioms that g ◦ f = idc and f ◦ g = idd?

In order for the above to be an isomorphism we need to have idc = idd i.e. g . f = f . g = id.
An example of two isomorphic types would be ((),a) and a for some a. We can construct the two morphisms:

> let f :: ((),a) -> a
>     f ((),x) = x

> let g :: a -> ((),a)
>     g x = ((),x)

And we can check that f . g == id == g . f.
