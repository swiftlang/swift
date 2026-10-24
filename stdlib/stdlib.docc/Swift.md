# ``Swift``

@Metadata {
    @DisplayName("Standard Library")
}

Browse the essential types provided by Swift’s standard library.

## Overview

The Swift standard library defines a base layer of functionality for writing Swift
programs, including:

- Fundamental data types such as ``Swift/Int``, ``Swift/Double``, and ``Swift/String``
- Common data structures such as ``Swift/Array``, ``Swift/Dictionary``, and ``Swift/Set``
- Global functions such as ``Swift/print(_:separator:terminator:)`` and ``Swift/abs(_:)``
- Protocols, such as ``Swift/Collection`` and ``Swift/Equatable``, that describe
common abstractions, customize operations, or replace boilerplate code

```swift
var interestingNumbers = [
    "primes": [2, 3, 5, 7, 11, 13, 17],
    "triangular": [1, 3, 6, 10, 15, 21, 28],
    "hexagonal": [1, 6, 15, 28, 45, 66, 91]
]

for key in interestingNumbers.keys {
    interestingNumbers[key]?.sort(by: >)
}

print(interestingNumbers["primes"]!)
// Prints "[17, 13, 11, 7, 5, 3, 2]"
```

> Note: Experiment with Swift standard library types and learn high-level concepts
using visualizations and practical examples. Learn how the Swift standard library
uses protocols and generics to express powerful constraints. Download the playground
below to get started.
>
> [Swift Standard Library.playground](https://developer.apple.com/sample-code/swift/downloads/standard-library.zip)

## Topics

### Values and collections

- <doc:numbers-and-basic-values>
- <doc:strings-and-text>
- <doc:collections>
- <doc:time-and-duration>

### Tools for your types

- <doc:basic-behaviors>
- <doc:encoding-decoding-and-serialization>
- <doc:initialization-with-literals>

### Data modeling

- <doc:choosing-between-structures-and-classes>
- <doc:adopting-common-protocols>

### Programming tasks

- <doc:input-and-output>
- <doc:debugging-and-reflection>
- <doc:macros>
- <doc:concurrency>
- <doc:key-path-expressions>
- <doc:manual-memory-management>
- <doc:type-casting-and-existential-types>
- <doc:operator-declarations>

### Language interoperability

- <doc:c-interoperability>
- <doc:cxx-interoperability>
- <doc:customizing-your-c-code-for-swift>
- <doc:using-imported-c-structs-and-unions-in-swift>
- <doc:using-imported-c-functions-in-swift>
- <doc:using-imported-c-macros-in-swift>

### Related frameworks

- <doc:Observation-collection>
- <doc:Distributed-collection>
- <doc:RegexBuilder-collection>
- <doc:Synchronization-collection>

### Deprecated

- <doc:deprecated>
