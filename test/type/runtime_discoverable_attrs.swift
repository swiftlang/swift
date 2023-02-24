// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/RuntimeAttrs.swift -o %t
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature RuntimeDiscoverableAttrs -I %t

// REQUIRES: asserts

import RuntimeAttrs

@runtimeMetadata
struct Flag<T> {
  init(attachedTo: T.Type, _ description: String = "") {}
  init<Args>(attachedTo: (Args) -> T, _ description: String = "") {}
  init<Base>(attachedTo: KeyPath<Base, T>, _ description: String = "") {}
}

@runtimeMetadata
struct OnlyPropsTest<B, V> {
  init(attachedTo: KeyPath<B, V>) {}
}

@runtimeMetadata
struct NoInitAttr { // expected-error {{runtime attribute type 'NoInitAttr' does not contain a required initializer - init(attachedTo:)}}
}

@runtimeMetadata
struct FailiableInit {
  init?(attachedTo: Any) {} // expected-error {{runtime attribute type initializer 'init(attachedTo:)' cannot be failable}}
}

@runtimeMetadata
public struct AccessMismatchInAttr {
  private init(attachedTo: Any) {}
  // expected-error@-1 {{private initializer 'init(attachedTo:)' cannot have more restrictive access than its enclosing runtime attribute type 'AccessMismatchInAttr' (which is public)}}
  internal init<T>(attachedTo: T.Type, other: Int) {}
  // expected-error@-1 {{internal initializer 'init(attachedTo:other:)' cannot have more restrictive access than its enclosing runtime attribute type 'AccessMismatchInAttr' (which is public)}}
}

@Flag("global") func gloabalFn() {}

@runtimeMetadata
struct WithOuterParam<T> {
  init<U>(attachedTo: U.Type) {}
  init<U>(attachedTo: U.Type, otherData: T) {}
}

@WithOuterParam<Int>
struct ExplicitGenericParamsTest1 {} // Ok

@WithOuterParam<Int>(otherData: 42)
struct ExplicitGenericParamsTest3 {} // Ok

@WithOuterParam(otherData: "")
struct ExplicitGenericParamsTest4 {} // Ok

@WithOuterParam<Int>(otherData: "") // expected-error {{cannot convert value of type 'String' to expected argument type 'Int'}}
struct ExplicitGenericParamsTest5 {}

@Flag
struct A { // Ok
  @Flag("v1") var v1: String = "" // Ok

  @Flag var comp: Int { // Ok
    get { 42 }
    @Flag set {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
  }

  @Flag static var v2: String = ""
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag static func test1() -> Int { 42 } // Ok
  @Flag("test2") func test2() {} // Ok

  @Flag func genericFn<T>(_: T) {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @OnlyPropsTest @Flag("x") var x: [Int]? = [] // Ok

  class Inner {
    @OnlyPropsTest @Flag("test property") var test: [Int]? = nil // Ok

    @Flag func testInner() {}
    @Flag static func testStaticInner() {}
  }
}

struct Context<T> {
  @Flag struct B {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag let x: Int = 0 // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
  @Flag subscript(v: Int) -> Bool { false }
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag func fnInGenericContext() {}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
}

do {
  @Flag let x: Int = 42 // expected-warning {{}}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag func localFn() {}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
}

@Flag @Flag func test() {} // expected-error {{duplicate runtime discoverable attribute}}

extension A.Inner {
  @Flag("B type") struct B { // Ok
    @Flag static func extInnerStaticTest() {} // Ok
    @Flag static func extInnerTest() {} // Ok

    @Flag let stored: Int = 42 // Ok
  }

  @Flag static func extStaticTest() {} // Ok
  @Flag static func extTest() {} // Ok

  @OnlyPropsTest @Flag("computed in extension") var extComputed: Int { // Ok
    get { 42 }
  }
}

@Flag func test(_: Int) {} // Ok
@Flag func test(_: String) {} // Ok

struct TestNoAmbiguity {
  @Flag static func testStatic() -> Int {} // Ok
  @Flag static func testStatic() {} // Ok

  @Flag func testInst(_: Int, _: String) {} // Ok
  @Flag func testInst(_: Int, _: Int) {} // Ok
}

@Flag
protocol Flagged {}

@Flag
protocol OtherFlagged {}

@Flag("flag from protocol") // expected-error {{reflection metadata attributes applied to protocols cannot have additional attribute arguments}}
protocol InvalidFlagged {}

struct Inference1 : Flagged {} // Ok
class Inference2 : Flagged {}  // Ok
enum Inference3 : Flagged {}   // Ok

@Flag("direct flag")
struct Inference4 : Flagged {} // Ok (because @Flag inferred from Flagged is ignored)

@available(macOS, introduced: 42.0)
@Flag("with availability")
func testWithAvailability(_: Int) {} // Ok

@available(macOS, introduced: 100.0)
class TypeWithAvailability : Flagged {} // Ok

class MembersWithAvailability {
  @Flag
  @available(macOS, introduced: 100.0)
  static func staticFn() {} // Ok

  @Flag
  @available(macOS, introduced: 100.0)
  func instFn() {} // Ok

  @Flag
  @available(macOS, introduced: 100.0)
  var prop: Int { get { 0 } } // Ok
}

func test_local_types_with_conformances() {
  struct X : Flagged {}
}

@runtimeMetadata
struct AttrWithInitsInExts {
}

extension AttrWithInitsInExts {
  init(attachedTo: Any.Type) {}
}

@AttrWithInitsInExts
struct TestAttrWithExts { // Ok
}

@runtimeMetadata
struct FlagForMutating {
  init<T, Result>(attachedTo: (inout T) -> Result) {}
  init<T, Result>(attachedTo: (inout T, String, inout Int, (String, [Int])) -> Result) {}
}

struct TestMutatingMethods {
  @FlagForMutating mutating func noArgs() -> Int { 42 } // Ok
  @FlagForMutating mutating func yesArgs(_: String, x: inout Int, _ data: (String, [Int])) -> (q: String, a: Int) {
    (q: "", a: 42)
  }
}

@runtimeMetadata
struct FlagForAsyncFuncs {
  init<Act>(attachedTo: (Act) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int, inout [Int]) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int) async -> Void) {}
  init(attachedTo: () async -> [String]) {}
}

actor TestActor {
  @FlagForAsyncFuncs func asyncExternally() throws {
  }

  @FlagForAsyncFuncs func doSomething() async throws {
  }

  @FlagForAsyncFuncs nonisolated func doSomething(_: Int) async {
  }

  @FlagForAsyncFuncs func doSomething(_: Int, x: inout [Int]) async {
  }
}

@FlagForAsyncFuncs
func globalAsyncFn() async -> [String] {
  return []
}

@runtimeMetadata
struct FlagForStaticFuncs {
  init<T>(attachedTo: (T.Type) -> Void) {}
  init<T>(attachedTo: (T.Type, Int.Type) -> Void) {}
  init<T>(attachedTo: (T.Type, inout [String], Int) -> Void) {}
}

struct TestStaticFuncs {
  @FlagForStaticFuncs static func test0() {}
  @FlagForStaticFuncs static func test1(_: Int.Type) {}
  @FlagForStaticFuncs static func test2(_: inout [String], x: Int) {}
}

struct TestSelfUse {
  static var description: String = "TestSelfUse"

  @Flag(Self.description) var x: Int = 42
  @Flag(Self.description) func test() {}
}


@runtimeMetadata
enum EnumFlag<B, V> {
  case type(B.Type)
  case method((B) -> V)
  case property(KeyPath<B, V>)
  case function(() -> V)
}

@EnumFlag
protocol EnumFlagged {}

extension EnumFlag {
  init(attachedTo: KeyPath<B, V>) { self = .property(attachedTo) }
  init(attachedTo: @escaping (B) -> V) { self = .method(attachedTo) }
}

extension EnumFlag where V == Void {
  init(attachedTo: B.Type) { self = .type(attachedTo) }
}

extension EnumFlag where B == Void {
  init(attachedTo: @escaping () -> V) { self = .function(attachedTo) }
}

@EnumFlag func globalEnumTest() -> (Int, [String])? {
  nil
}

@EnumFlag struct EnumTypeTest {
  @EnumFlag var x: Int = 42
  @EnumFlag func testInst() {}
  @EnumFlag static func testStatic() -> Int { 42 }
}

@Flag extension EnumTypeTest { // expected-error {{@Flag can only be applied to unavailable extensions in the same module as 'EnumTypeTest'}}
}

@available(*, unavailable)
@Flag extension EnumTypeTest { // Ok
}

@available(*, unavailable)
@EnumFlag extension EnumTypeTest { // expected-error {{@EnumFlag is already applied to type 'EnumTypeTest'; did you want to remove it?}} {{275:1-11=}}
}

@available(*, unavailable)
@Flag extension CrossModuleTest { // expected-error {{@Flag can only be applied to unavailable extensions in the same module as 'CrossModuleTest'}}
}

struct InvalidConformanceTest1 {}
struct InvalidConformanceTest2 {}

extension InvalidConformanceTest1 : Flagged {} // expected-error {{type 'InvalidConformanceTest1' does not conform to protocol 'Flagged'}}
// expected-note@-1 {{protocol 'Flagged' requires reflection metadata attribute @Flag}}

extension InvalidConformanceTest2 : Flagged & EnumFlagged {}
// expected-error@-1 {{type 'InvalidConformanceTest2' does not conform to protocol 'Flagged'}}
// expected-error@-2 {{type 'InvalidConformanceTest2' does not conform to protocol 'EnumFlagged'}}
// expected-note@-3 {{protocol 'Flagged' requires reflection metadata attribute @Flag}}
// expected-note@-4 {{protocol 'EnumFlagged' requires reflection metadata attribute @EnumFlag}}

@Flag
struct ValidConformance1 {}
extension ValidConformance1 : Flagged {} // Ok

struct ValidConformance2 : Flagged {}
extension ValidConformance2 : OtherFlagged {} // Ok (@Flag is inferred from Flagged protocol conformance)

@EnumFlag @Flag
class MultiAttrTest {
  init() {}
}

extension MultiAttrTest : Flagged & EnumFlagged {} // Ok

@Flag
class BaseClass {}

@Flag
class ValidChild : BaseClass {} // Ok

class InvalidChild : BaseClass {}
// expected-error@-1 {{superclass 'BaseClass' requires reflection metadata attribute @Flag}}
// expected-note@-2 {{add missing reflection metadata attribute @Flag}} {{1-1=@Flag }}
// expected-note@-3 {{opt-out of reflection metadata attribute @Flag using unavailable extension}} {{34-34=\n\n@available(*, unavailable)\n@Flag extension InvalidChild {\}\n}}

class OptedOutChild : BaseClass {} // Ok (used unavailable extension to opt-out of the attribute)

@available(*, unavailable)
@Flag
extension OptedOutChild {}

protocol InvalidProtoWithSuperclass : BaseClass {}
// expected-error@-1 {{superclass 'BaseClass' requires reflection metadata attribute @Flag}}
// expected-note@-2 {{add missing reflection metadata attribute @Flag}} {{1-1=@Flag }}
// expected-note@-3 {{opt-out of reflection metadata attribute @Flag using unavailable extension}} {{51-51=\n\n@available(*, unavailable)\n@Flag extension InvalidProtoWithSuperclass {\}\n}}

@Flag
protocol ValidProtoWithSuperclass : BaseClass {} // Ok

protocol OptedOutProtoWithSuperclass : BaseClass {} // Ok

@available(*, unavailable)
@Flag
extension OptedOutProtoWithSuperclass {}

@runtimeMetadata
struct FlagForFailures {
  init(attachedTo: Int.Type) {}
  init<T>(attachedTo: KeyPath<Int, T>) {}
  init(attachedTo: (Int) -> Void, _: String) {}
}

@FlagForFailures // expected-error {{cannot convert value of type 'InvalidMembersTests.Type' to expected argument type 'Int.Type'}}
struct InvalidMembersTests {
  @FlagForFailures var x: Int
  // expected-error@-1 {{cannot convert value of type 'KeyPath<InvalidMembersTests, Int>' to expected argument type 'KeyPath<Int, Int>'}}
  // expected-note@-2 {{arguments to generic parameter 'Root' ('InvalidMembersTests' and 'Int') are expected to be equal}}

  @FlagForFailures("") static func testStatic() {}
  // expected-error@-1 {{cannot convert value of type '(InvalidMembersTests.Type) -> ()' to expected argument type '(Int) -> Void'}}

  @FlagForFailures(42) func testInst() {}
  // expected-error@-1 {{cannot convert value of type '(InvalidMembersTests) -> ()' to expected argument type '(Int) -> Void'}}
  // expected-error@-2:20 {{cannot convert value of type 'Int' to expected argument type 'String'}}
}

@FlagForFailures("global") func testGlobalInvalid() {}
// expected-error@-1 {{cannot convert value of type '() -> ()' to expected argument type '(Int) -> Void'}}
