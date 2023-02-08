<!-- idris
module README

import Control.Monad.State
import Control.Monad.State.Tuple

import Data.String

%default total
-->

# Tuple instances for the standard MTL interfaces

## Multiple similar interfaces

You may know that Idris (unlike, say, Haskell) can easily handle several
occurrences of the same interface with different arguments,
including standard MTL ones like `MonadReader`, `MonadState` and `MonadWriter`
(assuming, the types are significantly different, of course).

For example:

```idris
multiS : Monad m =>
         MonadState Nat m =>
         MonadState String m =>
         Nat ->
         m ()
multiS m = do
  n <- get
  s <- get
  modify (++ replicate n '.')
  put $ m + length s
```

Compiler can figure out that the first `get` call and the `put` call are for the `MonadState Nat m`,
and that the second call to `get` and the `modify` call are for `MonadState String m`.
It manages to do so using types.

> **Note**
>
> Surely, you shouldn't use such simple types as `Nat` and `String` in `MonadState` in real code.
> I used them only for easiness of illustration.

## Running

But how would you run such a code?

Surely, you can invent some datatype `X` that would contain desired `Nat` and `String`,
and implement both `MonadState Nat` and `MonadState String` for, say, `StateT X m`...
But that's a lot of code which can be generalised!

One can use tuples instead!
Imagine of `StateT (Nat, String) m` would have `MonadState Nat` and `MonadState String` implementations.

```idris
main : IO Unit
main = do
  (n, s) <- execStateT (the Nat 2, "str") $ multiS 14
  putStrLn "nat: \{show n}, str: \{show s}"
```

What do you think would be printed?
You can check out [here](/tests/docs/readme/expected) ;-)

With this library you have an ability to use standard transformers like `StateT` or `RWST` with tupled state
implementing `MonadState` for each individual component.

## Flexibility

Surely, you are not limited to have only actually used things in the state.
For example, for following also works:

<!-- idris
%ambiguity_depth 4
%hide String.(::)
-->

```idris
main' : IO Unit
main' = do
  (n, c, s, xs) <- execStateT (the Nat 2, 'k', "str", [1, 2, 3]) $ multiS 14
  putStrLn "nat: \{show n}, char: \{show c}, str: \{show s}, list: \{show xs}"
```

## Others

Not only `MonadState` is supported, you can find also such tupled implementations of `MonadReader` for `ReaderT` and
`MonadWriter` for `WriterT`.
Also, all these interfaces are supported for `RWST`.

## Effectiveness

Surely, this is generally, permanent repacking.
Moreover, the deeper (to the right) the value is, the slower its operations are.

Well, I consider this to be the cost of simplicity of use.
