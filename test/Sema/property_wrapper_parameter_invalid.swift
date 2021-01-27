// RUN: %target-typecheck-verify-swift

@propertyWrapper
struct NonMutatingWrapper<T> {
  var wrappedValue: T {
    get { fatalError() }
    nonmutating set { fatalError() }
  }

  init(wrappedValue: T) { fatalError() }
}

@propertyWrapper
struct MutatingWrapper<T> {
  var wrappedValue: T {
    mutating get { fatalError() }
  }

  init(wrappedValue: T) { fatalError() }
}

// expected-error@+1 {{property wrapper applied to parameter must have a nonmutating 'wrappedValue' getter}}
func testMutatingGetter(@MutatingWrapper value: Int) {}

// expected-error@+1 {{property wrapper applied to parameter must have a nonmutating 'wrappedValue' getter}}
func testComposedMutating(@MutatingWrapper @NonMutatingWrapper value: Int) {}

// okay
func testComposedNonMutating(@NonMutatingWrapper @MutatingWrapper value: Int) {}
