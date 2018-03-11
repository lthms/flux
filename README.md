# Flux

**Flux** is intended to be an arrowized Haskell library which emphasises
concurrency.

At the core of **Flux** is three actors, implemented as Monadic Stream
Functions: `Producer`, `Flux` and `Consumer`.

* A `Producer i` generates values of type `i`
* A `Flux i o` is a treatment which turns a value of `i` into a value of `o`
* A `Consumer o` handle values of type `o`

**Flux** provides several functions to create new concurrent tasks using those
components. To connect these tasks together, it introduces two types: `Source`
and `Sink`.

* A `Source a` identifies a concurrent tasks which produces
values of a type `a`
* A `Sink a` identifies a concurrent tasks which consumes values of a type `a`

Finally, the `Flux` arrow is implemented such that it tries to leverages Haskell
concurrent-capabilities, in particular in its `Applicative` and `Arrow`
instances.
