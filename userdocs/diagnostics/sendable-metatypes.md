# Sendable metatypes

Types that are shared in concurrent code generally need to conform to `Sendable`. The same is true in generic code when sharing parameters of a generic parameter `T`. For example, the given code will produce an error under strict concurrency checking

```swift
func doSomethingElsewhere<T>(_ value: T) {
  Task { @concurrent in
    print(value) // warning: capture of non-Sendable type 'T'
  }
}
```

because `value` can have a non-Sendable type that is not safe to share. To address this potential data race, the type `T` can be marked as `Sendable`:

```swift
func doSomethingElsewhere<T: Sendable>(_ value: T) {
  Task { @concurrent in
    print(value)
  }
}
```

The same issue can occur when passing the type `T` itself, rather than a value of type `T`. The compiler will indicate such problems by noting that the metatype of `T`, spelled `T.Type`, is not `Sendable`:

```swift
protocol P {
  static func doSomething()
}

func doSomethingStatic<T: P>(_: T.Type) {
  Task { @concurrent in
    T.doSomething() // warning: capture of non-Sendable type 'T.Type' in an isolated closure
  }
}
```

In these cases, the type parameter should be required to conform to the `SendableMetatype` protocol, e.g.,

```swift
func doSomethingStatic<T: P & SendableMetatype>(_: T.Type) {
  Task { @concurrent in
    T.doSomething()
  }
}
```

The `SendableMetatype` requirement allows the function to share the type `T` in concurrent code. To maintain data race safety, it prevents callers from using isolated conformances in the call. For example, the following code will be rejected due to a data race:

```swift
@MainActor
class C: @MainActor P {
  static func doSomething() { }
}

@MainActor
func test(c: C) {
  doSomethingStatic(C.self) // error: main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter
}

```

The conformance of `C` to `P` can only be used on the main actor, so it cannot be provided to `doSomethingStatic`, which calls the conformance from a different concurrent task.