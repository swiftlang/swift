// RUN: %target-swift-frontend -typecheck -primary-file %s -verify -module-name main

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(wrappedValue initialValue: T) {
    wrappedValue = initialValue
  }
}

// expected-error@+1{{property wrappers are not yet supported in top-level code}}
@Wrapper var value: Int = 17

func f() { }
f()
