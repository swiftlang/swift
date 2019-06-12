// RUN: %target-typecheck-verify-swift

// expect-no-diagnostics

@propertyDelegate
struct Delegate<T> {
  var value: T
}

@propertyWrapper
struct Wrapper<T> {
  var value: T
}
