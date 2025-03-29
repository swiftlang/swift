// RUN: %target-typecheck-verify-swift

struct Projection<T> {
  var value: T
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T

  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  var projectedValue: Projection<T> {
    Projection(value: wrappedValue)
  }

  init(projectedValue: Projection<T>) {
    self.wrappedValue = projectedValue.value
  }
}

@propertyWrapper
struct ImplementationDetailWrapper<T> {
  var wrappedValue: T

  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

func globalFunc(@Wrapper arg: Int) {
  let _: Int = arg
  let _: Projection<Int> = $arg
  let _: Wrapper<Int> = _arg
}

func testGlobalFunc(value: Int, projection: Projection<Int>) {
  globalFunc(arg: value)
  globalFunc($arg: projection)

  let _: (Int) -> Void = globalFunc
  let _: (Int) -> Void = globalFunc(arg:)
  let _: (Projection<Int>) -> Void = globalFunc($arg:)
}

func globalFuncWithImplementationDetailWrapper(@ImplementationDetailWrapper arg: Int) {
  let _: Int = arg
  let _: ImplementationDetailWrapper<Int> = _arg
}

func testGlobalFuncWithImplementationDetailWrapper(value: Int) {
  globalFuncWithImplementationDetailWrapper(arg: value)

  let _: (Int) -> Void = globalFuncWithImplementationDetailWrapper
  let _: (Int) -> Void = globalFuncWithImplementationDetailWrapper(arg:)
}

struct S<Value> {
  func method(@Wrapper arg: Value) {
    let _: Value = arg
    let _: Projection<Value> = $arg
    let _: Wrapper<Value> = _arg
  }

  func methodWithImplementationDetailWrapper(@ImplementationDetailWrapper arg: Value) {
    let _: Value = arg
    let _: ImplementationDetailWrapper<Value> = _arg
  }

  static func staticMethod(@Wrapper arg: Value) {
    let _: Value = arg
    let _: Projection<Value> = $arg
    let _: Wrapper<Value> = _arg
  }

  static func staticMethodWithImplementationDetailWrapper(@ImplementationDetailWrapper arg: Value) {
    let _: Value = arg
    let _: ImplementationDetailWrapper<Value> = _arg
  }
}

func testMethods(instance: S<String>, Metatype: S<String>.Type,
                 @Wrapper value: String) {
  Metatype.staticMethod(arg: value)
  Metatype.staticMethod($arg: $value)

  instance.method(arg: value)
  instance.method($arg: $value)

  let _: (String) -> Void = Metatype.staticMethod
  let _: (String) -> Void = Metatype.staticMethod(arg:)
  let _: (Projection<String>) -> Void = Metatype.staticMethod($arg:)

  let _: (String) -> Void = instance.method
  let _: (String) -> Void = instance.method(arg:)
  let _: (Projection<String>) -> Void = instance.method($arg:)

  let _: (String) -> Void = instance.method
  let _: (String) -> Void = instance.method(arg:)
  let _: (Projection<String>) -> Void = instance.method($arg:)

  let _: (S) -> (String) -> Void = Metatype.method
  let _: (S) -> (String) -> Void = Metatype.method(arg:)
  let _: (S) -> (Projection<String>) -> Void = Metatype.method($arg:)
}

func testMethodsWithImplementationDetailWrapper(instance: S<String>, Metatype: S<String>.Type,
                                                @ImplementationDetailWrapper value: String) {
  Metatype.staticMethodWithImplementationDetailWrapper(arg: value)

  instance.methodWithImplementationDetailWrapper(arg: value)

  let _: (String) -> Void = Metatype.staticMethodWithImplementationDetailWrapper
  let _: (String) -> Void = Metatype.staticMethodWithImplementationDetailWrapper(arg:)

  let _: (String) -> Void = instance.methodWithImplementationDetailWrapper
  let _: (String) -> Void = instance.methodWithImplementationDetailWrapper(arg:)

  let _: (S) -> (String) -> Void = Metatype.methodWithImplementationDetailWrapper
  let _: (S) -> (String) -> Void = Metatype.methodWithImplementationDetailWrapper(arg:)
}

func testClosures() {
  typealias PropertyWrapperTuple = (Wrapper<Int>, Int, Projection<Int>)

  let _: (Int) -> PropertyWrapperTuple = { (@Wrapper value) in
    (_value, value, $value)
  }

  let _: (Projection<Int>) -> PropertyWrapperTuple = { (@Wrapper $value) in
    (_value, value, $value)
  }
}

func testClosuresWithImplementationDetailWrapper() {
  let _: (Int) -> (ImplementationDetailWrapper<Int>, Int) = { (@ImplementationDetailWrapper value) in
    (_value, value)
  }
}

func projectionPlaceholder<T>(@Wrapper _ value: T) {}

func testOmittedProjectionLabel(value: Int) {
    projectionPlaceholder($_: Projection(value: value))
}

@propertyWrapper
struct ProjectionWrapper<Value> {
  var wrappedValue: Value

  var projectedValue: ProjectionWrapper<Value> { self }

  init(wrappedValue: Value) { self.wrappedValue = wrappedValue }

  init(projectedValue: ProjectionWrapper<Value>) {
    self.wrappedValue = projectedValue.wrappedValue
  }
}

// https://github.com/swiftlang/swift/issues/77823
// Make sure we correctly handle compound applied functions.
func testCompoundApplication() {
  func foo(@ProjectionWrapper x: Int) {}
  struct HasProjectionWrapperMember {
    static func foo(@ProjectionWrapper x: Int) {}
  }

  foo(x:)(0)
  foo($x:)(ProjectionWrapper(wrappedValue: 0))

  (foo($x:).self)(ProjectionWrapper(wrappedValue: 0))
  HasProjectionWrapperMember.foo($x:)(ProjectionWrapper(wrappedValue: 0))

  foo(x:)(ProjectionWrapper(wrappedValue: 0)) // expected-error {{cannot convert value of type 'ProjectionWrapper<Int>' to expected argument type 'Int'}}
  foo(x:)(ProjectionWrapper(wrappedValue: "")) // expected-error {{cannot convert value of type 'ProjectionWrapper<String>' to expected argument type 'Int'}}
  foo(x:)("") // expected-error {{cannot convert value of type 'String' to expected argument type 'Int'}}

  foo($x:)(ProjectionWrapper(wrappedValue: "")) // expected-error {{cannot convert value of type 'String' to expected argument type 'Int'}}
  foo($x:)(0) // expected-error {{cannot convert value of type 'Int' to expected argument type 'ProjectionWrapper<Int>'}}
  foo($x:)("") // expected-error {{cannot convert value of type 'String' to expected argument type 'ProjectionWrapper<Int>'}}

  func bar(x: Int) {} // expected-note 2{{parameter 'x' does not have an attached property wrapper}}
  bar($x:)(0) // expected-error {{cannot use property wrapper projection argument}}
  _ = bar($x:) // expected-error {{cannot use property wrapper projection argument}}

  func baz(@ProjectionWrapper x: Int, @ProjectionWrapper y: Int) {}
  baz($x:y:)(ProjectionWrapper(wrappedValue: 0), 0)
  baz(x:$y:)(0, ProjectionWrapper(wrappedValue: 0))

  let _: (ProjectionWrapper<Int>, Int) -> Void = baz($x:y:)
  let _: (Int, ProjectionWrapper<Int>) -> Void = baz(x:$y:)
}

func testImplicitPropertyWrapper() {
  typealias PropertyWrapperTuple = (ProjectionWrapper<Int>, Int, ProjectionWrapper<Int>)

  let _: (ProjectionWrapper<Int>) -> PropertyWrapperTuple = { $value in
    (_value, value, $value)
  }
}

@resultBuilder
struct PairBuilder {
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
}

func takesResultBuilder<Projection, T1, T2>(projection: Projection,
                                            @PairBuilder _ closure: (Projection) -> (T1, T2)) {}

func testResultBuilderWithImplicitWrapper(@ProjectionWrapper value: String) {
  takesResultBuilder(projection: $value) { $value in
    value
    $value
  }
}

func takesWrapperClosure<T>(_: ProjectionWrapper<[S<T>]>, closure: (ProjectionWrapper<S<T>>) -> Void) {}

func testGenericPropertyWrapper<U>(@ProjectionWrapper wrappers: [S<U>]) {
  takesWrapperClosure($wrappers) { $wrapper in }
}

@propertyWrapper
struct Binding<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: Binding<Value> {
    return self
  }

  public init(projectedValue: Binding<Value>) {
    self = projectedValue
  }
}

struct Widget {
  init(@ProjectionWrapper w: Int) {}
}

func buildWidget(_ w: ProjectionWrapper<Int>) -> Widget {
  Widget($w: w)
}
