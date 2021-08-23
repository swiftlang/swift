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

func globalFunc(@Wrapper arg: Int) {
  let _: Int = arg
  let _: Projection<Int> = $arg
  let _: Wrapper<Int> = _arg
}

func testGloablFunc(value: Int, projection: Projection<Int>) {
  globalFunc(arg: value)
  globalFunc($arg: projection)

  let _: (Int) -> Void = globalFunc
  let _: (Int) -> Void = globalFunc(arg:)
  let _: (Projection<Int>) -> Void = globalFunc($arg:)
}


struct S<Value> {
  func method(@Wrapper arg: Value) {
    let _: Value = arg
    let _: Projection<Value> = $arg
    let _: Wrapper<Value> = _arg
  }

  static func staticMethod(@Wrapper arg: Value) {
    let _: Value = arg
    let _: Projection<Value> = $arg
    let _: Wrapper<Value> = _arg
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

func testClosures() {
  typealias PropertyWrapperTuple = (Wrapper<Int>, Int, Projection<Int>)

  let _: (Int) -> PropertyWrapperTuple = { (@Wrapper value) in
    (_value, value, $value)
  }

  let _: (Projection<Int>) -> PropertyWrapperTuple = { (@Wrapper $value) in
    (_value, value, $value)
  }
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
