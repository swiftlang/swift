# Potentially unstable witness for `static var shared` (UnstableGlobalActorShared)

## Overview

When declaring a global actor with `@globalActor`, the concrete instance of the actor used for isolation is returned by
`static var shared`. This property must always return the same actor instance in order for actor isolation to work correctly,
and establish the exact 1:1 mapping between `MyGlobalActor.shared` and `@MyGlobalActor` isolation.

Only by using a `static let` is this property guaranteed by the language. 

When the property is implemented as a stored `var` (which can be reassigned), 
or computed property (`var shared: T { ... }`) (which can return a different instance upon access),
this guarantee must be enforced dynamically by the implementation.

As the compiler cannot prove this implementation is correct, and it is critically important for actor isolation correctness,
the compiler emits a warning for those cases.

The following implementation is incorrect:

```swift
@globalActor
public actor MyGlobalActor {
  // Incorrect! New instance per every invocation
  public static var shared: MyGlobalActor { MyGlobalActor() } 
}
```

The correct way to implement this is as follows:

```swift
@globalActor
public actor MyGlobalActor {
  public static let shared = MyGlobalActor()
}
```

If the witness must be a computed property, because of initialization requirements of the shared actor,
or because the actor is a generic type which would prevent using a `let` constant, 
you can safely silence this warning as follows:

```swift
@globalActor
public actor MyGlobalActor {
  @diagnose(UnstableGlobalActorShared, as: ignored)
  public static var shared: MyGlobalActor { _shared }

  private static let _shared = MyGlobalActor()
}
```
