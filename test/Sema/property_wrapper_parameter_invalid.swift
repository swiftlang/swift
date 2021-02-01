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

@propertyWrapper
struct NoProjection<T> {
  var wrappedValue: T
}

func takesNoProjectionWrapper(@NoProjection value: String) {}

func testNoProjection(message: String) {
  takesNoProjectionWrapper(value: message) // okay

  // expected-error@+1 {{cannot use property wrapper projection parameter; wrapper 'NoProjection<String>' does not have a 'projectedValue'}}
  takesNoProjectionWrapper($value: message)
}

struct Projection<T> {
  var value: T
}

@propertyWrapper
struct Wrapper<T> {
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  init(projectedValue: Projection<T>) {
    self.wrappedValue = projectedValue.value
  }

  var wrappedValue: T
  var projectedValue: Projection<T> { Projection(value: wrappedValue) }
}

func hasWrapperAttributeArg<T>(@Wrapper() value: T) {}

func testWrapperAttributeArg(projection: Projection<Int>) {
  hasWrapperAttributeArg(value: projection.value)

  // expected-error@+1 {{cannot use property wrapper projection argument; pass wrapped value type 'Int' instead}}
  hasWrapperAttributeArg($value: projection)
}
