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

// expected-note@+1 {{property wrapper type 'NoProjection<String>' does not support initialization from a projected value}}
func takesNoProjectionWrapper(@NoProjection value: String) {}

func testNoProjection(message: String) {
  takesNoProjectionWrapper(value: message) // okay

  // expected-error@+1 {{cannot use property wrapper projection argument}}
  takesNoProjectionWrapper($value: message)

  // expected-error@+2 {{cannot use property wrapper projection parameter}}
  // expected-note@+1 {{property wrapper type 'NoProjection<Int>' does not support initialization from a projected value}}
  let _: (NoProjection<Int>) -> Int = { $value in
    return value
  }
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

// expected-note@+2 {{property wrapper has arguments in the wrapper attribute}}
// expected-note@+1 {{in call to function 'hasWrapperAttributeArg(value:)'}}
func hasWrapperAttributeArg<T>(@Wrapper() value: T) {}

func testWrapperAttributeArg(projection: Projection<Int>) {
  hasWrapperAttributeArg(value: projection.value)

  // expected-error@+2 {{cannot use property wrapper projection argument}}
  // expected-error@+1 {{generic parameter 'T' could not be inferred}}
  hasWrapperAttributeArg($value: projection)
}

struct S {
  // expected-error@+1 {{property wrapper attribute on parameter not yet supported on subscript}}
  subscript(@Wrapper position: Int) -> Int { 0 }
}

func testInvalidArgLabel() {
  // expected-note@+1 2 {{parameter 'argLabel' does not have an attached property wrapper}}
  func noWrappers(argLabel: Int) {}

  // expected-error@+1 {{cannot use property wrapper projection argument}}
  let ref = noWrappers($argLabel:)

  // expected-error@+1 {{cannot use property wrapper projection argument}}
  noWrappers($argLabel: 10)
}

protocol P {
  // expected-error@+1 {{parameter 'arg' declared inside a protocol cannot have a wrapper}}
  func requirement(@Wrapper arg: Int)
}
