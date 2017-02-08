// RUN: not %swift -typecheck -target %target-triple %s -emit-fixits-path %t.remap -I %S/Inputs
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

class Base {}
class Derived : Base {}

var b : Base
b as Derived
b as Derived

b as! Base

var opti : Int?
// Add bang.
var i : Int = opti
// But remove unnecessary bang.
var i2 : Int = i!

struct MyMask : OptionSet {
  init(_ rawValue: UInt) {}
  init(rawValue: UInt) {}
  init(nilLiteral: ()) {}

  var rawValue: UInt { return 0 }

  static var allZeros: MyMask { return MyMask(0) }
  static var Bingo: MyMask { return MyMask(1) }
}

func supported() -> MyMask {
  return Int(MyMask.Bingo.rawValue)
}

struct MyEventMask2 : OptionSet {
  init(rawValue: UInt64) {}
  var rawValue: UInt64 { return 0 }
}
func sendIt(_: MyEventMask2) {}
func sendItOpt(_: MyEventMask2?) {}
func sendItOpt3(_: MyEventMask2???) {}
func testMask1(a: Int) {
  sendIt(a)
}
func testMask2(a: UInt64) {
  sendIt(a)
}
func testMask3(a: MyEventMask2) {
  testMask1(a: a)
}
func testMask4(a: MyEventMask2) {
  testMask2(a: a)
}
func testMask5(a: Int) {
  sendItOpt(a)
}
func testMask6(a: Int) {
  sendItOpt(a)
}
func testMask7(a: Int?) {
  sendItOpt(a)
}
func testMask8(a: UInt64?) {
  sendItOpt(a)
}
func testMask9(a: Any) {
  sendItOpt(a as? Int)
}
func testMask10(a: Int?) {
  sendIt(a) // no fix, nullability mismatch.
}
func testMask11(a: MyEventMask2?) {
  testMask7(a: a)
}
func testMask12(a: MyEventMask2?) {
  testMask8(a: a)
}
func testMask13(a: MyEventMask2?) {
  testMask1(a: a) // no fix, nullability mismatch.
}
func testMask14() {
  sendIt(1)
  sendItOpt(2)
}

struct Wrapper {
  typealias InnerMask = MyEventMask2
}
func sendItInner(_: Wrapper.InnerMask) {}
func testInnerMask(a: UInt64) {
  sendItInner(a)
}

struct SomeName : RawRepresentable {
  init(_ rawValue: String) {}
  init(rawValue: String) {}
  var rawValue: String { return "" }
}
func testPassSomeName(_: SomeName) {}
func testConvertSomeName(s: String) {
  testPassSomeName("\(s)}")
}

class WrappedClass {}
class WrappedClassSub: WrappedClass {}
struct ClassWrapper : RawRepresentable {
  var rawValue: WrappedClass
}
func testPassAnyObject(_: AnyObject) {}
func testPassAnyObjectOpt(_: AnyObject?) {}
func testPassWrappedSub(_: WrappedClassSub) {}
func testConvertClassWrapper(_ x: ClassWrapper, _ sub: WrappedClassSub) {
  testPassAnyObject(x)
  testPassAnyObjectOpt(x)
  testPassWrappedSub(x)

  let iuo: ClassWrapper! = x
  testPassAnyObject(iuo)
  testPassAnyObjectOpt(iuo)

  let _: ClassWrapper = sub
  let _: ClassWrapper = x.rawValue
  // FIXME: This one inserts 'as!', which is incorrect.
  let _: ClassWrapper = sub as AnyObject
}

enum MyEnumType : UInt32 {
  case invalid
}
_ = MyEnumType(MyEnumType.invalid)

func goo(var e : Error) {
}
func goo2(var e: Error) {}
func goo3(var e: Int) { e = 3 }
protocol A {
  func bar(var s: Int)
}
extension A {
  func bar(var s: Int) {
    s += 5
  }
}

func baz(var x: Int) {
  x += 10
}
func foo(let y: String, inout x: Int) {

}

struct Test1 : OptionSet {
  init(rawValue: Int) {}
  var rawValue: Int { return 0 }
}

print("", false)

func ftest1() {
  // Don't replace the variable name with '_'
  let myvar = 0
}

func ftest2(x x: @escaping Int -> Int) {}

protocol SomeProt {
  func protMeth(p: Int)
}
@objc protocol SomeObjCProt {
  func objcprotMeth(p: Int)
}
class Test2 : SomeProt, SomeObjCProt {
  func protMeth(_ p: Int) {}

  func instMeth(p: Int) {}
  func instMeth2(p: Int, p2: Int) {}
  func objcprotMeth(_ p: Int) {}
}
@objc class Test3 : SomeObjCProt {
  func objcprotMeth(_ p: Int) {}
}
class SubTest2 : Test2 {
  override func instMeth(_ p: Int) {}
}
Test2().instMeth(0)
Test2().instMeth2(0, p2:1)

func recit(_: Int32) {}
func ftest3(_ fd: CInt) {
  recit(UInt(fd))
}
func ftest4(_ fd: UInt) {
  recit(fd)
}

func letToVar1() {
  let x = 1
  if x == 2 {
    x += 3
  }
  let y = ""
  y.append("as")
  y.append("df")
}

class Node  {}
class Graph<NodeType : Node> {}
var graph: Graph

class Node2  {}
class Graph2<NodeType1 : Node, NodeType2 : Node2> {}
var graph: Graph2

@objc protocol ObjCProt { }
class Graph3<NodeType : ObjCProt> {}
var graph: Graph3

class Graph4<NodeType : SomeProt> {}
var graph: Graph4
var graphAgain = Graph4()

class GraphCombo<NodeType : SomeProt & ObjCProt> {}
var graph: GraphCombo

func evilCommas(s: String) {
  _ = s[s.startIndex..<<#editorplaceholder#>]
  _ = true ? s[s.startIndex..<<#editorplaceholder#>] : ""
  _ = [s.startIndex..<<#editorplaceholder#>]
}

import Empty
func testGenericSig(x: Empty<Int>) -> Empty<String> {}

class NonObjC {}
protocol NonObjCProtocol {}
@objc class IBIssues {
  @IBOutlet static private var ibout1: IBIssues!
  @IBOutlet private var ibout2: NonObjC!
  @IBOutlet private var ibout3: NonObjCProtocol!
  @IBOutlet private let ibout4: IBIssues!
  @IBOutlet private var ibout5: [[IBIssues]]!
  @IBOutlet private var ibout6: [String: String]!
  @IBInspectable static private var ibinspect1: IBIssues!
  @IBAction static func ibact() {}
}

@IBDesignable extension SomeProt {}

func attrNowOnType(foo: ()->()) {}

class InitDynType {
  init() {}
  func notInit() {
    self.init()
  }
}

class NoSemi {
  enum Bar { case bar }
  var foo: .Bar = .bar
}

func fnWithClosure(c: @escaping ()->()) {}
func testescape(rec: ()->()) {
  fnWithClosure { rec() }
}

@warn_unused_result func testDeprecatedAttr() -> Int { return 0 }

protocol Prot1 {}
protocol Prot2 {
  associatedtype Ty = Prot1
}
class Cls1 : Prot1 {}
func testwhere<T: Prot2 where T.Ty == Cls1>(_: T) {}

enum E {
  case abc
}
func testEnumRename() { _ = E.Abc }

func testAnyToAnyObject(x: Any) {
  x.instMeth(p: 1)
}

func testProtocolCompositionSyntax() {
  var _: protocol<>
  var _: protocol<Prot1>
  var _: protocol<Prot1, Prot2>
}

func disable_unnamed_param_reorder(p: Int, _: String) {}
disable_unnamed_param_reorder(0, "") // no change.

prefix operator ***** {}

class BoolFoo : BooleanType {
  var boolValue: Bool {return false}
}
func testBoolValue(a : BoolFoo) {
  if a { }
  guard a {}
  if a as BoolFoo {}
}

protocol P1 {}
protocol P2 {}
var a : protocol<P1, P2>?
var a2 : protocol<P1>= 17

class TestOptionalMethodFixit {
  optional func test() {}
}
