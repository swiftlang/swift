// This test is by no means exhaustive, but attempts to catch any places in the
// implementation of ASTDumper's JSON where it might do an unprotected call to
// some method on AST node that would cause an assertion due to some
// unsatisfied precondition. This is a good place to put regression tests if
// issues are discovered in the future.
//
// This file should not contain top-level code, since it contains a `@main`
// type. It should also not contain code that requires Obj-C; put that in
// ast-dump-json-objc-no-crash.swift instead.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-swift-5.9-abi-triple -swift-version 6 -I %S/Inputs/dependencies -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o - >/dev/null

struct E: Error {}

@main
struct MainThing {
    static func main() {}
}

// Statements

func f1() {
    return ()
}
func f2() {
    defer { print() }
    if true { print() } else { print() }
    guard true else { fatalError() }
    while true { print() }
    do { print() }
    do { throw E() } catch let e1 as E { _ = e1 } catch { _ = error }
    repeat { print() } while true
    for _ in [] where true { }
    for case let .some(x) in [1, nil, 3] where x % 2 == 0 { }

    switch Int.random(in: 0...10) {
    case 0: fallthrough
    case 1...6: break
    case let x where x % 2 == 0: break
    default: break
    }

    label1: for _ in [] { continue label1 }
    label2: for _ in [] { break label2 }

    struct S1: ~Copyable {
        deinit {}
        consuming func f() { discard self }
    }
    struct S2 {
        init?() { return nil }
    }
}

// Declarations

protocol P1 {
    var v1: Int { get set }
    var v2: Int { get set }
    func f()

    associatedtype A1
    associatedtype A2 = Int
    associatedtype A3: BinaryInteger where A3: Codable
}
protocol P2: AnyObject where Self: C1 {}

public final class C1: P1 {
    public typealias A1 = String
    public typealias A3 = Int32

    public var v1: Int {
        willSet { print() }
        didSet { print() }
    }
    public var v2: Int {
        get { 10 }
        set { print() }
    }
    public init() {
        self.v1 = 0
        self._v3 = 0
    }
    public func f() {}

    public var _v3: Int
    public var v3: Int {
        @storageRestrictions(accesses: v1, initializes: _v3)
        init { _v3 = 10 }
        get { 0 }
        set { print() }
    }
}
open class C2 {
    public required init() {}
    func f1() {}
    class func f2() {}
    static func f3() {}

    class var v2: Int { 0 }
    nonisolated(unsafe) static var v3: Int = 0
    static var v4: Int { 0 }
}
class C3: C2 {
    required init() { super.init() }
    override func f1() {}
    override class func f2() { super.f2() }
}
actor AC1 {
    nonisolated func f1() {}
}
enum E1 {
    indirect case a(E1)
    case b, c(Int), d(x: Int, y: String = "")
}
indirect enum E2 { case e1(E2), e2 }
struct S1<A, B: BinaryInteger> where A: StringProtocol {
    func f1() where A == String, B == Int {}
}
typealias TA1<B> = S1<String, B> where B: FixedWidthInteger

@MainActor public struct MAS {}

@MainActor protocol MAP { func f() }
struct MASInferred: MAP {
    func f() {}
}

@dynamicMemberLookup
struct DynLook {
    subscript(dynamicMember member: String) -> String { return member }

    func f() {
        _ = self.hello
    }
}
func z1() {
    _ = DynLook().hello
}

@dynamicCallable
struct DynCall {
    func dynamicallyCall(withKeywordArguments args: KeyValuePairs<String, String>) {}

    func f() {
        self(label: "value")
    }
}
func z2() {
    let dc = DynCall()
    dc(label: "value")
}

func z3() {
    var s1: S1<Substring, UInt>
    var ta1: TA1<Int32>
}
func f1() -> some BinaryInteger { 0 }
func f2<T: BinaryInteger>(_ t: T) {}
func f3(_ t: some BinaryInteger) {}
func f4(_ values: Int...) {}
func z4() {
    f4(1, 2, 3)
}

func f5( _ value: @autoclosure () -> Int) {}
func z5() {
    f5(10)
}

func f6(_ x: Int = 10) {}
func f7(_ x: inout Int) { x = 50 }
func z7() {
    var f7x = 10; f7(&f7x)
}

func f8() async throws -> Int { 0 }
func f9() async throws {
    _ = try await f8()
    async let x = f8()
    print(try await x)
}

func f10(isolation: any Actor = #isolation) {}

struct LazyHolder {
  lazy var v: Int = 10
}

struct Accessors {
  private var _x: Int
  var x: Int {
    _read { yield _x }
    _modify { yield &_x }
  }
}

struct SynthEq: Equatable {
  var x: Int
  var y: String
}
struct SynthHash: Hashable {
  var x: Int
  var y: String
}
struct SynthCode: Codable {
  var x: Int
  var y: String
}
enum SynthComp: Comparable {
    case a
    case b(Int)
    case c(Int, String)
}

import Swift
import struct Swift.Int
@preconcurrency import Swift
public import Swift
import UserClangModule

@freestanding(expression)
macro Macro1<T: BinaryInteger>(t: T) -> String = #externalMacro(module: "DummyModule", type: "DummyType")

@attached(member)
macro Macro2<T: BinaryInteger>(t: T) = #externalMacro(module: "DummyModule", type: "DummyType")

struct ToExtend<T> {}
extension ToExtend {
    func f1() {}
}
extension ToExtend where T == Int {
    func f2() {}
}
extension [Int] {
    func f3() {}
}

precedencegroup MooglePrecedence {
    higherThan: AdditionPrecedence
    associativity: left
    assignment: false
}
infix operator ^*^: MooglePrecedence

@resultBuilder
struct StringBuilder {
    static func buildBlock(_ parts: String...) -> String {
        parts.joined()
    }
}
@StringBuilder func sb1() -> String {
    "hello"
    " "
    "world"
}
func sb2(@StringBuilder _ body: () -> String) { print(body()) }
func sb3() {
    sb2 {
        "hello"
        " "
        "world"
    }
}

func nestedOpaques0() -> some BinaryInteger { 2 }
func nestedOpaques1() -> some FixedWidthInteger & SignedInteger { 2 }
func nestedOpaques2() -> (some BinaryInteger, some Sequence) { (2, []) }
func nestedOpaques3() -> (some BinaryInteger, some Sequence<Double>) { (2, []) }
func nestedOpaques4() -> (some BinaryInteger)? { Bool.random() ? 2 : nil }
func nestedOpaques5() -> [some BinaryInteger] { [2] }

// Expressions

func zz1() throws {
    let _: Unicode.Scalar = "a"
    let _: Character = "a\u{0301}"
    _ = "abc"
    _ = true
    _ = 10
    _ = 0x10
    _ = 10.0
    _ = 0x10p4
    _ = "a\(10)b"
    _ = #file
    _ = #line
    _ = /hello(?<capture>\s+)world/
    _ = Array<Int>(repeating: 0, count: 2)

    let array1 = [1, 2, 3]
    _ = array1[0]

    let dict1 = ["a": 1, "b": 2, "c": 3]
    _ = dict1["a"]

    let tup1 = (1, b: 2, c: 3)
    _ = tup1.0
    _ = tup1.b

    let j: Int = 4
    _ = (1 + 9) * -3 / j << 2

    struct CAF {
        func callAsFunction(x: Int) {}
    }
    let caf = CAF()
    caf(x: 10)

    struct KP {
        var x: KP2?
    }
    struct KP2 {
        var y: Int
    }
    let kp = KP()
    _ = kp[keyPath: \.x?.y]
    _ = kp[keyPath: \KP.x?.y]

    let cv = 5
    _ = copy cv

    struct NonCop: ~Copyable {}
    let noncop = NonCop()
    _ = consume noncop

    func thrower() throws(E) -> Int { throw E() }
    _ = try thrower()
    _ = try? thrower()
    _ = try! thrower()

    _ = { [kp, cv] in
    print(kp, cv)
    }
    _ = { @MainActor @Sendable (x: Int) -> String in
    return "\(x)"
    }
    _ = { (x: Int) -> Int in
    try thrower()
    }
    _ = { (x: Int) -> String in
    try await f9()
    return "ok"
    }
    let _: (Int, Int) -> Int = { $0 + $1 }
}

struct Pack<each T> {
    func f(_ t: repeat each T) {
        repeat g(each t)
    }
    func g<U>(_ t: U) {}
}

func tuplify<each T>(_ value: repeat each T) -> (repeat each T) {
  return (repeat each value)
}
func example<each T>(_ value: repeat each T) {
  let abstractTuple = tuplify(repeat each value)
  repeat print(each abstractTuple)
}

func anySeq<T>(_ type: T.Type = T.self) -> any Sequence<T> { [] }
func anySeqUser() {
    let s = anySeq(Int.self)
    let iter = s.makeIterator()
}
func opaqueSeq<T>(_ type: T.Type = T.self) -> some Sequence<T> { [] }
func opaqueSeqUser() {
    let s = opaqueSeq(Int.self)
    let iter = s.makeIterator()
}

let x = 10
func zz1b() {
    _ = type(of: x)
}

struct RebindInit {
    var x: Int
    init() { self = .init(x: 0)}
    init(x: Int) { self.x = x }
}

@propertyWrapper
struct PropWrap<T> {
    var wrappedValue: T
    var projectedValue: PropWrap<T> { self }

    init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}
struct WrapperHolder {
    @PropWrap var w: Int = 0

    mutating func f() {
        self.w = 0
        self._w = PropWrap(wrappedValue: 1)
        _ = self.$w
    }
}

func zz2() {
    struct S3 {
        var x: Int
    }
    let s3: S3? = nil
    _ = s3?.x.bigEndian
    _ = s3!.x.bigEndian

    _ = 5 as Int
    _ = 5 as? String
    _ = 5 as! Double

    let te1: Int
    let te2: String
    (te1, te2) = (1, "hello")

    let an1: Any
    an1 = 5

    let trn = Bool.random() ? 10 : 20
}

protocol P9 {
    func f() -> Self
}
func doSomething(_ p: any P9) {
    p.f()  // implicit opening
}

func zz3() {
    let ah: AnyHashable = 10
}

// Patterns

enum ForPattern {
    case a
    case b(value: Int, name: String)
    case c(value: Int, name: String)
    case d(Int)
    case e(Bool)
}
func ffp(_ value: ForPattern) {
    switch value {
    case .a: break
    case .b(let v, let n): print(v, n); break
    case let .c(v, n): print(v, n); break
    case .d(let x) where x > 10: break
    case .d(20): break
    case .d: break
    case .e(true): break
    case .e(false): break
    }
    switch 5 {
    case is Int: break
    case _: break;
    }
}
func ffpo(_ value: ForPattern?) {
    switch value {
    case .a?: break
    case .d(x as Int): break
    default: break
    }
}

// Various other constructs

@available(iOS, deprecated: 14.0, renamed: "newThing()", message: "Use the new thing")
func oldThing() {}

@available(iOS, introduced: 14.0)
func newThing() {}

func newThingClient() {
    if #available(iOS 14.0, *) {
        newThing()
    } else {
        oldThing()
    }
}

dynamic func toBeReplaced(arg: Int) {}

@_dynamicReplacement(for: toBeReplaced(arg:))
func toReplaceWith(arg: Int) {}
