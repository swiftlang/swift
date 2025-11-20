// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

// Rerun with optimizations to check if -O does not make any difference
// RUN: %target-swift-frontend -O -emit-sil -primary-file %s -o /dev/null -verify

func ifFalse() -> Int {
  if 1 == 0 { // expected-note {{always evaluates to false}}
    return 0 // expected-warning {{will never be executed}}
  } else {
    return 1
  }
}

func ifTrue() -> Int {
  _ = 0
  if 1 == 1 { // expected-note {{always evaluates to true}}
    return 1
  }
  return 0 // expected-warning {{will never be executed}}
}

func testUnreachableIfBranch() -> Int {
  let a = 2
  let c: Int
  if a < 2 {  // expected-note {{condition always evaluates to false}}
    c = 3     // expected-warning {{will never be executed}}
  } else {
    c = 4
  }
  return c
}

func testUnreachableIfBranch2() -> Int {
  let a = 2
  let c: Int
  if a > 2 { // expected-note {{condition always evaluates to false}}
    c = 3    // expected-warning {{will never be executed}}
  } else {
    c = 4
  }
  return c
}

func testUnreachableElseBranch() -> Int {
  let a = 2
  let c: Int
  if a == 2 { // expected-note {{condition always evaluates to true}}
    c = 3
  } else {
    c = 4     // expected-warning {{will never be executed}}
  }
  return c
}

// FIXME: False Negative: <rdar://39516135>. No warnings are produced here
// as the statements along the unreachable branches are marked implicit.
// Unreachable code analysis suppresses warnings in such cases.
func testQuestionMarkOperator() -> Int {
  let a = 2
  let c: Int
  c = (a < 2) ? 3 : 4
  return c
}

// Work-around <rdar://problem/17687851> by ensuring there is
// something that appears to be user code in unreachable blocks.
func userCode() {}

func whileTrue() {
  var x = 0
  while 1 == 1 { // expected-note {{always evaluates to true}}
    x += 1
  }
  userCode() // expected-warning {{will never be executed}}
}

func whileTrueSilent() {
  while true {
  }
}   // no warning!

func whileTrueReachable(_ v: Int) -> () {
  var x = 0
  while true {
    if v == 0 {
      break
    }
    x += 1
  }
  x -= 1
}

func whileTrueTwoPredecessorsEliminated() -> () {
  var x = 0
  while (1 == 1) { // expected-note {{always evaluates to true}}
    if false {
      break
    }
    x += 1
  }
  userCode()  // expected-warning {{will never be executed}}
}

func unreachableBranch() -> Int {
  if 1 == 0 { // expected-note {{always evaluates to false}}
    // FIXME: It'd be nice if the warning were on 'if true' instead of the 
    // body.
    if true {
      return 0 // expected-warning {{will never be executed}}
    } 
  } else {
    return 1  
  }
}

// We should not report unreachable user code inside inlined transparent function.
@_transparent
func ifTrueTransparent(_ b: Bool) -> Int {
  _ = 0
  if b {
    return 1
  }
  return 0
}
func testIfTrueTransparent() {
  _ = ifTrueTransparent(true)  // no-warning
  _ = ifTrueTransparent(false)  // no-warning
}

// We should not report unreachable user code inside generic instantiations.
// TODO: This test should start failing after we add support for generic 
// specialization in SIL. To fix it, add generic instantiation detection 
// within the DeadCodeElimination pass to address the corresponding FIXME note.
protocol HavingGetCond {
  func getCond() -> Bool
}
struct ReturnsTrue : HavingGetCond {
  func getCond() -> Bool { return true }
}
struct ReturnsOpaque : HavingGetCond {
  var b: Bool
  func getCond() -> Bool { return b }
}
func ifTrueGeneric<T : HavingGetCond>(_ x: T) -> Int {
  if x.getCond() {
    return 1
  }
  return 0
}
func testIfTrueGeneric(_ b1: ReturnsOpaque, b2: ReturnsTrue) {
  _ = ifTrueGeneric(b1)  // no-warning
  _ = ifTrueGeneric(b2)  // no-warning
}

// Test switch_enum folding/diagnostic.
enum X {
  case One
  case Two
  case Three
}

func testSwitchEnum(_ xi: Int) -> Int {
  var x = xi
  let cond: X = .Two
  switch cond { // expected-warning {{switch condition evaluates to a constant}}
  case .One:
    userCode() // expected-note {{will never be executed}}
  case .Two:
    x -= 1
  case .Three:
    x -= 1
  }

  switch cond { // no warning
  default:
    x += 1
  }

  switch cond { // expected-warning{{switch condition evaluates to a constant}}
  case .Two: 
    x += 1
  default: 
    userCode() // expected-note{{will never be executed}}
  }

  switch cond { // expected-warning{{switch condition evaluates to a constant}}
  case .One: 
    userCode() // expected-note{{will never be executed}}
  default: 
    x -= 1
  }
  
  return x
}

@_silgen_name("exit") func exit() -> Never

func reachableThroughNonFoldedPredecessor(fn: @autoclosure () -> Bool = false) {
  if !_fastPath(fn()) {
    exit()
  }
  var _: Int = 0 // no warning
}

func intConstantTest() -> Int{
  let y: Int = 1
  if y == 1 { // expected-note {{condition always evaluates to true}}
    return y
  }
  
  return 1 // expected-warning {{will never be executed}}
}

func intConstantTest2() -> Int{
  let y:Int = 1
  let x:Int = y

  if x != 1 { // expected-note {{condition always evaluates to false}}
    return y // expected-warning {{will never be executed}}
  }
  return 3
}

func test_single_statement_closure(_ fn:() -> ()) {}
test_single_statement_closure() {
    exit() // no-warning
}

class C { }
class Super { 
  var s = C()
  deinit { // no-warning
  }
}
class D : Super { 
  var c = C()
  deinit { // no-warning
    exit()
  }
}



// <rdar://problem/20097963> incorrect DI diagnostic in unreachable code
enum r20097963Test {
  case A
  case B
}

class r20097963MyClass {
  func testStr(_ t: r20097963Test) -> String {
    let str: String
    switch t {
    case .A:
      str = "A"
    case .B:
      str = "B"
    default:    // expected-warning {{default will never be executed}}
      str = "unknown"  // Should not be rejected.
    }
    return str
  }
}

func die() -> Never { die() } // expected-warning {{function call causes an infinite recursion}}

func testGuard(_ a : Int) {
  guard case 4 = a else {  }  // expected-error {{'guard' body must not fall through, consider using a 'return' or 'throw'}}

  guard case 4 = a else { return }  // ok
  guard case 4 = a else { die() }  // ok
  guard case 4 = a else { fatalError("baaad") }  // ok

  for _ in 0...100 {
    guard case 4 = a else { continue } // ok
  }
}

func testFailingCast(_ s:String) -> Int {
   // There should be no notes or warnings about a call to a noreturn function, because we do not expose
   // how casts are lowered.
   return s as! Int // expected-warning {{cast from 'String' to unrelated type 'Int' always fails}}
}

enum MyError : Error { case A }

func raise() throws -> Never { throw MyError.A }

func test_raise_1() throws -> Int {
  try raise()
}

func test_raise_2() throws -> Int {
  try raise() // expected-note {{a call to a never-returning function}}
  try raise() // expected-warning {{will never be executed}}
}

// If a guaranteed self call requires cleanup, don't warn about
// release instructions
struct Algol {
  var x: [UInt8]

  func fail() throws -> Never { throw MyError.A }

  mutating func blah() throws -> Int {
    try fail() // no-warning
  }
}

class Lisp {
  func fail() throws -> Never { throw MyError.A }
}

func transform<Scheme : Lisp>(_ s: Scheme) throws {
  try s.fail() // no-warning
}

func deferNoReturn() throws {
  defer {
    _ = Lisp() // no-warning
  }

  die()
}

func deferTryNoReturn() throws {
  defer {
    _ = Lisp() // no-warning
  }

  try raise()
}

func noReturnInDefer() {
  defer { // expected-warning {{'defer' statement at end of scope always executes immediately}}{{3-8=do}}
    _ = Lisp()
    die() // expected-note {{a call to a never-returning function}}
    die() // expected-warning {{will never be executed}}
  }
}

while true {
}
 // no warning!


// rdar://25278336
// https://github.com/apple/swift/issues/43622
// Spurious 'will never be executed' warnings when building standard library

struct S_43622<T> {
  var a : T
}

extension S_43622 {
  @available(*, unavailable, message: "use the 'enumerated()' method on the sequence")
  init(_ base: Int) {
    fatalError("unavailable function can't be called")
  }
}

// More spurious 'will never be executed' warnings
struct FailingStruct {
  init?(x: ()) {
    fatalError("gotcha")
  }
}

class FailingClass {
  init?(x: ()) {
    fatalError("gotcha")
  }

  convenience init?(y: ()) {
    fatalError("gotcha")
  }
}

// https://github.com/apple/swift/issues/45333
// We should not report unreachable code inside protocol witness thunks

protocol Fooable {
  init()
  func foo() -> Never
}
struct Foo: Fooable {
  init() { // no-warning
    fatalError()
  }

  func foo() -> Never { // no-warning
    while true {}
  }
}

// We should not report unreachable code inside vtable thunks
class Base {
  required init(x: Int) {
    fatalError()
  }

  func foo(x: Int) -> Never {
    while true {}
  }
}

class Derived : Base {
  required init(x: Int?) {
    fatalError()
  }

  override func foo(x: Int?) -> Never {
    while true {}
  }
}

// Inout writeback
func takesInOut(value: inout SillyStruct) -> Never {
  while true {}
}

struct SillyStruct {
  mutating func mutatingMethod() -> Never {
    takesInOut(value: &self)
  }
}

// This triggers various problems
public func genericMightBeNever<R>(
  _ body: () -> R) -> R {
  while true {}

}

func sillyGenericExample() -> Never {
  return genericMightBeNever {
    return genericMightBeNever {
      return fatalError()
    }
  }
}

// https://github.com/apple/swift/issues/50015

protocol P {
    static var theThing: Self { get }
}

extension Never : P {
    static var theThing: Never { return fatalError() }
}

func test<T: P>(_ type: T.Type) -> T {
    return type.theThing
}

func f(i: Int?) {
    guard i != nil else { Never.theThing }
    guard i != nil else { test(Never.self) }
}

extension Collection {
  // Check that the destroy_addr which is inserted by DestroyHoisting does not
  // trigger a warning here.
  func f() -> Index {
    var lo = startIndex
    var hi = endIndex
    while true {
        formIndex(after: &lo)
        formIndex(after: &hi)
        if Bool.random() { return hi }
    }
  }
}

// rdar://80415811
// Incorrect unreachable code diagnostic for keypath to empty Enum caused by
// the generated keypath getter function having an uninhabited return type.
// The getter function was previously tied to the source location of the
// corresponding keypath expression, which caused it to be diagnosed as
// user code. The getter and setter functions now have an autogenerated
// source location so we should not expect an unreachable code warning until
// the keypath expression is actually used.
struct StructWithNeverProp {
  var property: Never {
    fatalError()
  }
}

func keypathToEmptyEnum() -> Never {
  // Check that the generated keypath getter function for this property
  // does not trigger an unreachable code warning here.
  let kp = \StructWithNeverProp.property // no warning
  let s = StructWithNeverProp()
  // Emit a diagnostic here because the keypath is actually used.
  let prop = s[keyPath: kp]
    // expected-warning@-1 {{will never be executed}} \
    // expected-note {{a call to a never-returning function}} \
    // expected-warning {{constant 'prop' inferred to have type 'Never', which is an enum with no cases}} \
    // expected-note {{add an explicit type annotation to silence this warning}}
  return prop
}

struct OuterStruct {
  public let innerEnum: InnerEnum
  public enum InnerEnum { }
}

@dynamicMemberLookup
enum DynamicLookupEnum {
    subscript<T>(dynamicMember keyPath: KeyPath<OuterStruct, T>) -> T {
        fatalError()
    }
}

func keypathWithDynamicLookup() {
  // Check that we still don't diagnose the keypath getter as unreachable
  // when used in conjunction with a dynamicMemberLookup enum.
  let _ = \DynamicLookupEnum.innerEnum // no warning
}

func test_no_warnings_with_fatalError_when_wrapped_in_buildExpression() {
  enum Either<T,U> {
    case first(T)
    case second(U)
  }

  @resultBuilder
  struct MyBuilder {
    static func buildExpression<T>(_ e: T) -> T { e }
    static func buildBlock() -> () { }

    static func buildBlock<T1>(_ t1: T1) -> T1 {
      return t1
    }

    static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
      return (t1, t2)
    }

    static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3) -> (T1, T2, T3) {
      return (t1, t2, t3)
    }

    static func buildEither<T,U>(first value: T) -> Either<T, U> {
      return .first(value)
    }

    static func buildEither<T,U>(second value: U) -> Either<T, U> {
      return .second(value)
    }
  }

  func test<T>(@MyBuilder _: (Int) -> T) {}

  test {
    if $0 < 0 {
      fatalError() // ok, no warning even though fatalError() is wrapped
    } else if $0 > 0 {
      42
    } else {
      0
    }
  }

  test {
    switch $0 {
    case 0: "0"
    default: fatalError() // Ok, no warning even though fatalError() is wrapped
    }
  }
}
