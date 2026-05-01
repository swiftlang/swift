# Calling a mutating async actor-isolated method (ActorIsolatedMutatingAsync)

## Overview

Calling a mutating async method on actor-isolated state allows concurrent access to that state during mutation, risking data races. To resolve this error, copy the state to a local variable, call the mutating async method on the copy, then write it back. This will overwrite concurrent changes.

For example:

```swift
struct Backpack {
  var snacks = 5
  mutating func refill() async {
    snacks += await purchaseSnacks(current: snacks)
  }
}

@MainActor
class Hiker {
  var backpack = Backpack()

  func getReady() async {
    await backpack.refill()
  }

  func eatSnack() {
    backpack.snacks -= 1
  }
}
```

Building the above code produces an error about calling a mutating async function on an actor-isolated property:

```
|   func getReady() async {
|     await backpack.refill()
|           |        `- error: cannot call mutating async function 'refill()' on actor-isolated property 'backpack' [#ActorIsolatedMutatingAsync]
|           `- note: 'backpack' can be concurrently accessed during mutation, risking data races
|   }
```

A `mutating async` function takes `self` as `inout`. This means that the value of `backpack` will be copied into `refill`, then copied out when the method returns. `inout` requires exclusive access for the duration of the call. While `refill` is suspended, the main actor is free to do other work. This is a data race, since `refill` is still mutating `backpack`. If `eatSnack` ran before `refill` finished, it would decrement `snacks`, but the eaten snack reappears when `refill` writes back the stale copy. 

```swift
// Task 1: getReady()                  | // Task 2
await backpack.refill()
// copies backpack (snacks: 5)         |
// suspends...                         |
                                         eatSnack()
//                                     | // backpack.snacks is now 4
// resumes                             |
// purchaseSnacks returns 0            |
// writes back backpack (snacks: 5)    |
// backpack.snacks is 5                |
// eatSnack() was silently lost!       |
```

When possible, you can structure code to avoid the need for mutating async functions by separating async computation and mutation:

```swift
func getReady() async {
  // await the asynchronous computation
  let refill = await backpack.computeRefill()
  // apply the mutation synchronously
  backpack.apply(refill)
}
```

Because the mutation is synchronous, no concurrent changes can occur between reading and writing `backpack` as part of `apply`. If `eatSnack` is called concurrently while `computeRefill` is suspended, the `Hiker` may purchase too few snacks, but the decrement from eating a snack won't be lost.

If restructuring isn't appropriate, copy `backpack` to a local variable to make the potential overwrite explicit at the call site. This way, the call site shows that `backpack` is overwritten after `await`, rather than happening implicitly due to `inout`. Because `copy` is a local variable, it is not shared with other tasks and can be safely mutated.

```swift
func getReady() async {
  var copy = backpack
  await copy.refill()
  backpack = copy
}
```

## See also

- [In-Out Parameters](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations#In-Out-Parameters)
- [Overlapping accesses, but operation requires exclusive access (ExclusivityViolation)](exclusivity-violation.md)
