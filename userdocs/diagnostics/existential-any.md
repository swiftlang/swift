# `ExistentialAny`

This diagnostic group includes errors and warnings pertaining to the `any` type
syntax.

This syntax was proposed in [SE-0335](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0335-existential-any.md).
`any` syntax draws a line between constraint types and existential or boxed
types.

For example, `any Collection` is a boxed type abstracting over a value of a
dynamic type that conforms to the protocol `Collection`, whereas the
`Collection` part is the conformance constraint imposed on the type of the
underlying value *as well as* a constraint type.
The distinction between a conformance constraint and a constraint type can be
clearly seen in classic generic syntax: `<T: Collection>`.

Constraint types exist to express conformances and have no meaning in relation
to values.

```swift
func sillyFunction(collection: Collection) { // error
  // ...
}
```
