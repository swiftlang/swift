// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

enum Either<T,U> {
  case first(T)
  case second(U)
}

@_functionBuilder
struct TupleBuilder {
  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildIf<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }

  static func buildArray<T>(_ array: [T]) -> [T] { return array }
}

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) throws -> T) rethrows {
  print(try body(cond))
}

// CHECK: (17, 3.14159, "Hello, DSL", (["nested", "do"], 6), Optional((2.71828, ["if", "stmt"])))
let name = "dsl"
tuplify(true) {
  17
  3.14159
  "Hello, \(name.map { $0.uppercased() }.joined())"
  do {
    ["nested", "do"]
    1 + 2 + 3
  }
  if $0 {
    2.71828
    ["if", "stmt"]
  }
}

// CHECK: ("Empty optional", nil)
tuplify(false) {
  "Empty optional"
  if $0 {
    2.71828
    ["if", "stmt"]
  }
}

// CHECK: ("chain0", main.Either<(Swift.String, Swift.Double), (Swift.Double, Swift.String)>.second(2.8, "capable"))
tuplify(false) {
  "chain0"
  if $0 {
    "marginal"
    2.9
  } else {
    2.8
    "capable"
  }
}

// CHECK: ("chain1", nil)
tuplify(false) {
  "chain1"
  if $0 {
    "marginal"
    2.9
  } else if $0 {
    2.8
    "capable"
  }
}

// CHECK: ("chain2", Optional(main.Either<(Swift.String, Swift.Double), (Swift.Double, Swift.String)>.first("marginal", 2.9)))
tuplify(true) {
  "chain2"
  if $0 {
    "marginal"
    2.9
  } else if $0 {
    2.8
    "capable"
  }
}

// CHECK: ("chain3", main.Either<main.Either<(Swift.String, Swift.Double), (Swift.Double, Swift.String)>, main.Either<(Swift.Double, Swift.Double), (Swift.String, Swift.String)>>.first(main.Either<(Swift.String, Swift.Double), (Swift.Double, Swift.String)>.first("marginal", 2.9)))
tuplify(true) {
  "chain3"
  if $0 {
    "marginal"
    2.9
  } else if $0 {
    2.8
    "capable"
  } else if $0 {
    2.8
    1.0
  } else {
    "wild"
    "broken"
  }
}

// CHECK: ("chain4", main.Either<main.Either<main.Either<(Swift.String, Swift.Int), (Swift.String, Swift.Int)>, main.Either<(Swift.String, Swift.Int), (Swift.String, Swift.Int)>>, main.Either<main.Either<(Swift.String, Swift.Int), (Swift.String, Swift.Int)>, (Swift.String, Swift.Int)>>.first
tuplify(true) {
  "chain4"
  if $0 {
    "0"
    0
  } else if $0 {
    "1"
    1
  } else if $0 {
    "2"
    2
  } else if $0 {
    "3"
    3
  } else if $0 {
    "4"
    4
  } else if $0 {
    "5"
    5
  } else {
    "6"
    6
  }
}

// rdar://50710698
// CHECK: ("chain5", 8, 9)
tuplify(true) {
  "chain5"
  #if false
    6
    $0
  #else
    8
    9
  #endif
}

// CHECK: ("getterBuilder", 0, 4, 12)
@TupleBuilder
var globalBuilder: (String, Int, Int, Int) {
  "getterBuilder"
  0
  4
  12
}
print(globalBuilder)

// CHECK: ("funcBuilder", 13, 45.0)
@TupleBuilder
func funcBuilder(d: Double) -> (String, Int, Double) {
  "funcBuilder"
  13
  d
}
print(funcBuilder(d: 45))

struct MemberBuilders {
  @TupleBuilder
  func methodBuilder(_ i: Int) -> (String, Int) {
    "methodBuilder"
    i
  }

  @TupleBuilder
  static func staticMethodBuilder(_ i: Int) -> (String, Int) {
    "staticMethodBuilder"
    i + 14
  }

  @TupleBuilder
  var propertyBuilder: (String, Int) {
    "propertyBuilder"
    12
  }
}

// CHECK: ("staticMethodBuilder", 27)
print(MemberBuilders.staticMethodBuilder(13))

let mbuilders = MemberBuilders()

// CHECK: ("methodBuilder", 13)
print(mbuilders.methodBuilder(13))

// CHECK: ("propertyBuilder", 12)
print(mbuilders.propertyBuilder)

// SR-11439: Operator builders
infix operator ^^^
func ^^^ (lhs: Int, @TupleBuilder rhs: (Int) -> (String, Int)) -> (String, Int) {
  return rhs(lhs)
}

// CHECK: ("hello", 6)
print(5 ^^^ {
  "hello"
  $0 + 1
})

struct Tagged<Tag, Entity> {
  let tag: Tag
  let entity: Entity
}

protocol Taggable {
}

extension Taggable {
  func tag<Tag>(_ tag: Tag) -> Tagged<Tag, Self> {
    return Tagged(tag: tag, entity: self)
  }
}

extension Int: Taggable { }
extension String: Taggable { }
extension Double: Taggable { }

@_functionBuilder
struct TaggedBuilder<Tag> {
  static func buildBlock() -> () { }

  static func buildBlock<T1>(_ t1: Tagged<Tag, T1>) -> Tagged<Tag, T1> {
    return t1
  }

  static func buildBlock<T1, T2>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>) -> (Tagged<Tag, T1>, Tagged<Tag, T2>) {
    return (t1, t2)
  }

  static func buildBlock<T1, T2, T3>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>, _ t3: Tagged<Tag, T3>)
      -> (Tagged<Tag, T1>, Tagged<Tag, T2>, Tagged<Tag, T3>) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>, _ t3: Tagged<Tag, T3>, _ t4: Tagged<Tag, T4>)
      -> (Tagged<Tag, T1>, Tagged<Tag, T2>, Tagged<Tag, T3>, Tagged<Tag, T4>) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>, _ t3: Tagged<Tag, T3>, _ t4: Tagged<Tag, T4>, _ t5: Tagged<Tag, T5>
  ) -> (Tagged<Tag, T1>, Tagged<Tag, T2>, Tagged<Tag, T3>, Tagged<Tag, T4>, Tagged<Tag, T5>) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildIf<T>(_ value: Tagged<Tag, T>?) -> Tagged<Tag, T>? { return value }
}

enum Color {
  case red, green, blue
}

func acceptColorTagged<Result>(@TaggedBuilder<Color> body: () -> Result) {
  print(body())
}

struct TagAccepter<Tag> {
  static func acceptTagged<Result>(@TaggedBuilder<Tag> body: () -> Result) {
    print(body())
  }
}

func testAcceptColorTagged(b: Bool, i: Int, s: String, d: Double) {
  // FIXME: When we support buildExpression, drop the "Color" prefix
  // CHECK: Tagged<
  acceptColorTagged {
    i.tag(Color.red)
    s.tag(Color.green)
    d.tag(Color.blue)
  }

  // FIXME: When we support buildExpression, drop the "Color" prefix
  // CHECK: Tagged<
  TagAccepter<Color>.acceptTagged {
    i.tag(Color.red)
    s.tag(Color.green)
    d.tag(Color.blue)
  }

  // CHECK: Tagged<
  TagAccepter<Color>.acceptTagged { () -> Tagged<Color, Int> in 
    if b {
      return i.tag(Color.green)
    } else {
      return i.tag(Color.blue)
    }
  }
}

testAcceptColorTagged(b: true, i: 17, s: "Hello", d: 3.14159)

// Use buildExpression() when it's available.
enum Component {
  case string(StaticString)
  case floating(Double)
  case color(Color)
  indirect case array([Component])
  indirect case optional(Component?)
}

@_functionBuilder
struct ComponentBuilder {
  static func buildExpression(_ string: StaticString) -> Component {
    return .string(string)
  }

  static func buildExpression(_ float: Double) -> Component {
    return .floating(float)
  }

  static func buildExpression(_ color: Color) -> Component {
    return .color(color)
  }

  static func buildBlock(_ components: Component...) -> Component {
    return .array(components)
  }

  static func buildIf(_ value: Component?) -> Component {
    return .optional(value)
  }
}

func acceptComponentBuilder(@ComponentBuilder _ body: () -> Component) {
  print(body())
}

func colorWithAutoClosure(_ color: @autoclosure () -> Color) -> Color {
  return color()
}

var trueValue = true
acceptComponentBuilder {
  "hello"
  if trueValue {
    3.14159
    colorWithAutoClosure(.red)
  }
  .red
}
// CHECK: array([main.Component.string("hello"), main.Component.optional(Optional(main.Component.array([main.Component.floating(3.14159), main.Component.color(main.Color.red)]))), main.Component.color(main.Color.red)])

// rdar://53325810

// Test that we don't have problems with expression pre-checking when
// type-checking an overloaded function-builder call.  In particular,
// we need to make sure that expressions in the closure are pre-checked
// before we build constraints for them.  Note that top-level expressions
// that need to be rewritten by expression prechecking (such as the operator
// sequences in the boolean conditions and statements below) won't be
// rewritten in the original closure body if we just precheck the
// expressions produced by the function-builder transformation.
struct ForEach1<Data : RandomAccessCollection, Content> {
  var data: Data
  var content: (Data.Element) -> Content

  func show() {
    print(content(data.first!))
    print(content(data.last!))
  }
}
extension ForEach1 where Data.Element: StringProtocol {
  // Checking this overload shouldn't trigger inappropriate caching that
  // affects checking the next overload.
  init(_ data: Data,
       @TupleBuilder content: @escaping (Data.Element) -> Content) {
    self.init(data: data, content: content)
  }
}
extension ForEach1 where Data == Range<Int> {
  // This is the overload we actually want.
  init(_ data: Data,
       @TupleBuilder content: @escaping (Int) -> Content) {
    self.init(data: data, content: content)
  }
}
let testForEach1 = ForEach1(-10 ..< 10) { i in
  "testForEach1"
  if i < 0 {
    "begin"
    i < -5
  } else {
    i > 5
    "end"
  }
}
testForEach1.show()

// CHECK: ("testForEach1", main.Either<(Swift.String, Swift.Bool), (Swift.Bool, Swift.String)>.first("begin", true))
// CHECK: ("testForEach1", main.Either<(Swift.String, Swift.Bool), (Swift.Bool, Swift.String)>.second(true, "end"))

func test_single_stmt_closure_support() {
  @_functionBuilder
  struct MyBuilder {
    static func buildBlock(_ numbers: Int...) -> Int {
      return 42
    }
  }

  func test(@MyBuilder builder: () -> Int) -> Int {
    builder()
  }

  let _ = test { 0 } // ok
}

// Check a case involving nested closures that refer to parameters of their
// enclosing closures.
struct X<C: Collection, T> {
  init(_ c: C, @TupleBuilder body: (C.Element) -> T) { }
}

struct Y<T> {
  init(@TupleBuilder body: () -> T) { }
}

struct Z<T> {
  init(@TupleBuilder body: () -> T) { }
}

func testNestedClosuresWithDependencies(cond: Bool) {
  tuplify(cond) { _ in
    X([1, 2, 3]) { x in
      Y {
        Z {
          x
          1
        }
      }
    }
  }
}

// Check that we can handle multiple conditions in an 'if' statement.
func testIfConditions(cond: Bool, c1: Bool, i1: Int, i2: Int) {
  tuplify(cond) { x in
    "testIfConditions"
    if i1 == i2, c1, x {
      1
      "hello"
    }
    3.14159
  }
}
testIfConditions(cond: true, c1: true, i1: 1, i2: 1)
// CHECK: testIfConditions
// CHECK-SAME: hello

// Use a "let" declaration within a function builder.
tuplify(true) { c in
  "testLetDeclarations"
  let (a, b) = (c, c && true)
  if a == b {
    "hello"
    b
  }
  a
}
// CHECK: testLetDeclarations"
// CHECK-SAME: hello
// CHECK-SAME: true

// Use if let / if case with various forms of decomposition.
func getOptionalInt(_: Bool) -> Int? { return 25 }

enum E {
  case a
  case b(Int, String?)
}

func getE(_ i: Int) -> E {
  switch i {
  case 0:
    return .a
  case 1:
    return .b(17, "hello")
  case 2:
    return .b(42, nil)
  default:
    fatalError("Unhandled case")
  }
}

tuplify(true) { c in
  "testIfLetMatching"
  if let theValue = getOptionalInt(c) {
    theValue + 17
  }
  if case let .a = getE(0) {
    "matched without payload"
  }
  if case let .b(i, s?) = getE(1) {
    "matched with payload"
    s + "!"
    i + 17
  }
  if case let .b(i, s?) = getE(2) {
    fatalError("cannot match this")
  } else {
    "intentional mismatch"
  }
}
// CHECK: testIfLetMatching
// CHECK-SAME: Optional(42)
// CHECK-SAME: Optional("matched without payload")
// CHECK-SAME: "matched with payload", "hello!", 34
// CHECK-SAME: "intentional mismatch"

class Super { }

class Sub : Super {
  func subMethod() -> String {
    return "subMethod"
  }
}

func getSuper(wantSubclass: Bool) -> Super {
  return wantSubclass ? Sub() : Super()
}

tuplify(true) { c in
  "testIfLetAsMatching"
  if case let sub as Sub = getSuper(wantSubclass: true) {
    sub.subMethod()
  }
  if case let sub as Sub = getSuper(wantSubclass: false) {
    fatalError("cannot match this")
  } else {
    "Superclass instance"
  }
}
// CHECK: testIfLetAsMatching
// CHECK-SAME: "subMethod"
// CHECK-SAME: "Superclass instance"


// switch statements
func testSwitch(_ e: E) {
  tuplify(true) { c in
    "testSwitch"
    switch e {
    case .a:
      "a"
    case .b(let i, let s?):
      i * 2
      s + "!"
    case .b(let i, nil):
      "just \(i)"
    }
  }
}

// CHECK: testSwitch
// CHECK-SAME: first(main.Either<Swift.String, (Swift.Int, Swift.String)>.first("a"))
testSwitch(getE(0))

// CHECK: testSwitch
// CHECK-SAME: first(main.Either<Swift.String, (Swift.Int, Swift.String)>.second(34, "hello!"))
testSwitch(getE(1))

// CHECK: testSwitch
// CHECK-SAME: second("just 42")
testSwitch(getE(2))

func testSwitchCombined(_ eIn: E) {
  var e = eIn
  tuplify(true) { c in
    "testSwitchCombined"
    switch e {
    case .a:
      "a"
    case .b(let i, _?), .b(let i, nil):
      "just \(i)"
    }
  }
}

// CHECK: testSwitchCombined
// CHECK-SAME: main.Either<Swift.String, Swift.String>.first("a")
testSwitchCombined(getE(0))

// CHECK: testSwitchCombined
// CHECK-SAME: second("just 17")
testSwitchCombined(getE(1))

// CHECK: testSwitchCombined
// CHECK-SAME: second("just 42")
testSwitchCombined(getE(2))


// Test buildOptional(_:) as an alternative to buildIf(_:).
@_functionBuilder
struct TupleBuilderWithOpt {
  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildOptional<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

func tuplifyWithOpt<T>(_ cond: Bool, @TupleBuilderWithOpt body: (Bool) -> T) {
  print(body(cond))
}
tuplifyWithOpt(true) { c in
  "1"
  3.14159
}

// Test for-each loops with buildArray.
// CHECK: testForEach
// CHECK-SAME: (1, "separator")
// CHECK-SAME: (2, "separator")
// CHECK-SAME: (3, "separator")
// CHECK-SAME: (4, "separator")
// CHECK-SAME: (5, "separator")
// CHECK-SAME: (6, "separator")
// CHECK-SAME: (7, "separator")
// CHECK-SAME: (8, "separator")
// CHECK-SAME: (9, "separator")
// CHECK-SAME: (10, "separator")
tuplify(true) { c in
  "testForEach"
  for i in 0 ..< (c ? 10 : 5) {
    i + 1
    "separator"
  }
}

// Test the use of function builders partly implemented through a protocol.
indirect enum FunctionBuilder<Expression> {
    case expression(Expression)
    case block([FunctionBuilder])
    case either(Either<FunctionBuilder, FunctionBuilder>)
    case optional(FunctionBuilder?)
}

protocol FunctionBuilderProtocol {
    associatedtype Expression
    typealias Component = FunctionBuilder<Expression>
    associatedtype Return

    static func buildExpression(_ expression: Expression) -> Component
    static func buildBlock(_ components: Component...) -> Component
    static func buildOptional(_ optional: Component?) -> Component
    static func buildArray(_ components: [Component]) -> Component
    static func buildLimitedAvailability(_ component: Component) -> Component

    static func buildFinalResult(_ components: Component) -> Return
}

extension FunctionBuilderProtocol {
    static func buildExpression(_ expression: Expression) -> Component { .expression(expression) }
    static func buildBlock(_ components: Component...) -> Component { .block(components) }
    static func buildOptional(_ optional: Component?) -> Component { .optional(optional) }
    static func buildArray(_ components: [Component]) -> Component { .block(components) }
    static func buildLimitedAvailability(_ component: Component) -> Component { component }
}

@_functionBuilder
enum ArrayBuilder<E>: FunctionBuilderProtocol {
    typealias Expression = E
    typealias Component = FunctionBuilder<E>
    typealias Return = [E]

    static func buildFinalResult(_ components: Component) -> Return {
        switch components {
        case .expression(let e): return [e]
        case .block(let children): return children.flatMap(buildFinalResult)
        case .either(.first(let child)): return buildFinalResult(child)
        case .either(.second(let child)): return buildFinalResult(child)
        case .optional(let child?): return buildFinalResult(child)
        case .optional(nil): return []
        }
    }
}


func buildArray(@ArrayBuilder<String> build: () -> [String]) -> [String] {
    return build()
}


let a = buildArray {
    "1"
    "2"
    if Bool.random() {
        "maybe 3"
    }
}
// CHECK: ["1", "2"
print(a)

// Throwing in function builders.
enum MyError: Error {
  case boom
}

// CHECK: testThrow
do {
  print("testThrow")
  try tuplify(true) { c in
    "ready to throw"
    throw MyError.boom
  }
} catch MyError.boom {
  // CHECK: caught it!
  print("caught it!")
} catch {
  fatalError("Threw something else?")
}

// CHECK: testStoredProperties
struct MyTupleStruct<T, U> {
  @TupleBuilder let first: () -> T
  @TupleBuilder let second: U
}

print("testStoredProperties")
let ts1 = MyTupleStruct {
  1
  "hello"
  if true {
    "conditional"
  }
} second: {
  3.14159
  "blah"
}

// CHECK: MyTupleStruct<(Int, String, Optional<String>), (Double, String)>(first: (Function), second: (3.14159, "blah"))
print(ts1)
