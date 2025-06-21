// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature Extern \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature RawLayout \
// RUN:   -enable-experimental-feature SymbolLinkageMarkers \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -enable-experimental-concurrency \
// RUN:   -enable-experimental-move-only \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature Extern \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature RawLayout \
// RUN:   -enable-experimental-feature SymbolLinkageMarkers \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -enable-experimental-concurrency \
// RUN:   -enable-experimental-move-only \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift \
// RUN:   -module-abi-name ASTGen \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature Extern \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature RawLayout \
// RUN:   -enable-experimental-feature SymbolLinkageMarkers \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -enable-experimental-concurrency \
// RUN:   -enable-experimental-move-only

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_RawLayout
// REQUIRES: swift_feature_SymbolLinkageMarkers
// REQUIRES: swift_feature_CDecl

// rdar://116686158
// UNSUPPORTED: asan

struct S1 {
  static func staticMethod() {}
}

func testStatic() {
  // static.
  S1.staticMethod()
  S1().staticMethod() // expected-error {{static member 'staticMethod' cannot be used on instance of type 'S1'}}
}

struct S2 {
  private func privateMethod() {} // expected-note {{'privateMethod()' declared here}}
  fileprivate func fileprivateMethod() {}
  internal func internalMethod() {}
  public func publicMethod() {}
}

func testAccessControl(value: S2) {
  // access control.
  value.privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
  value.fileprivateMethod()
  value.internalMethod()
  value.publicMethod()
}

struct S3 {
  mutating func mutatingMethod() {}
  func normalMethod() {}
}

func testMutating(value: S3) {
  value.mutatingMethod() // expected-error {{cannot use mutating member on immutable value}}
  value.normalMethod()
}

@frozen // expected-error {{'@frozen' attribute cannot be applied to this declaration}}
class C1 {}
@_alignment(7) // expected-error {{alignment value must be a power of two}}
struct S4 {}

@implementation extension ObjCClass1 {} // expected-error {{cannot find type 'ObjCClass1' in scope}}
@implementation(Category) extension ObjCClass1 {} // expected-error {{cannot find type 'ObjCClass1' in scope}}

@abi(func fn_abi()) // expected-error {{cannot give global function 'fn' the ABI of a global function with a different number of parameters}}
func fn(_: Int) {}

@_alignment(8) struct AnyAlignment {}

@_allowFeatureSuppression(IsolatedAny) public func testFeatureSuppression(fn: @isolated(any) @Sendable () -> ()) {}

@_disallowFeatureSuppression(NoncopyableGenerics) public struct LoudlyNC<T: ~Copyable> {}

@_cdecl("c_function_name") func cdeclUnderscore(x: Int) {}
@cdecl(c_function_name_official) func cdecl(x: Int) {}
@cdecl func cdeclDefault() {}

struct StaticProperties {
  dynamic var property: Int { return 1 }
}
extension StaticProperties {
  @_dynamicReplacement(for: property)
  var replacement_property: Int { return 2 }
}

@_documentation(visibility: internal) public protocol ProtocolShouldntAppear {}
@_documentation(metadata: cool_stuff) public class CoolClass {}
@_documentation(metadata: "this is a longer string") public class LongerMetadataClass {}

class EffectsTestClass {
}
struct EffectsTestStruct {
  var c: EffectsTestClass

  @_effects(releasenone) @_effects(readonly) // ok
  init(releasenone_readonly c: EffectsTestClass) { self.c = c }
}

public final class CustomEffectsTestClass {
  @usableFromInline
  var i = 27
  public init() { }
}
@_effects(notEscaping x.**)
public func noInlineWithEffects(_ x: CustomEffectsTestClass) -> Int {
  return x.i
}

class ExclusivityAttrClass {
  @exclusivity(unchecked) var instanceVar1: Int = 27
  @exclusivity(checked) var instanceVar2: Int = 27
}

@_extern(wasm, module: "m1", name: "f1") func externWasmFn(x: Int) -> Int
@_extern(c, "valid") func externCValidFn()
@_extern(c) func externCFn()

struct SectionStruct {
	@_section("__TEXT,__mysection") @_used func foo() {}
}

protocol ImplementsProto {
  func f0() -> Int
}
struct ImplementsStruct: ImplementsProto {
  @_implements(ImplementsProto, f0()) func foo() -> Int { 1 }
}

@inline(never) func neverInline() {}
@inline(__always) func alwaysInline() {}

@_nonSendable struct NonSendableWins: Sendable { }
@_nonSendable(_assumed) struct NonSendableLoses: Sendable { }

@_optimize(speed) func OspeedFunc() {}

@_private(sourceFile: "none") func foo() {} // expected-error {{@_private may only be used on 'import' declarations}} {{1-31=}}

struct ProjectedValueStruct {
  @_projectedValueProperty($dummy)
  let property: Never
}

@_semantics("foobar") func foobar() {}

@_silgen_name("silgen_func") func silGenFn() -> Int

@_specialize(where X: _TrivialStride(16), Y: _Trivial(32, 4), Z: _Class)
func testSpecialize<X, Y, Z>(x: X, y: Y, z: Z) {}

@specialized(where X == Int, Y == Float, Z == Double)
func testSpecializePublic<X, Y, Z>(x: X, y: Y, z: Z) {}

@_spi(SPIName) public func spiFn() {}

struct StorageRestrctionTest {
  var x: Int
  var y: String

  var _x: Int {
    @storageRestrictions(initializes: x, accesses: y)
    init(initialValue) {}
    get { x }
    set { }
  }
}

@_unavailableFromAsync struct UnavailFromAsyncStruct { } // expected-error {{'@_unavailableFromAsync' attribute cannot be applied to this declaration}}
@_unavailableFromAsync(message: "foo bar") func UnavailFromAsyncFn() {}

@concurrent func testGlobal() async { // Ok
}

do {
  nonisolated(nonsending) func testLocal() async {} // Ok

  struct Test {
    @concurrent func testMember() async {} // Ok
  }
}

typealias testConvention = @convention(c) (Int) -> Int
typealias testExecution = @concurrent () async -> Void
typealias testIsolated = @isolated(any) () -> Void

protocol OpProto {}
struct OpStruct: OpProto {}
struct OpTest {
  func opResult() -> some OpProto { OpStruct() }
  typealias Result = @_opaqueReturnTypeOf("$s6ASTGen6OpTestV8opResultQryF", 0) __
}

struct E {}
struct NE : ~Escapable {}
@_lifetime(copy ne) func derive(_ ne: NE) -> NE { ne }
@_lifetime(borrow ne1, copy ne2) func derive(_ ne1: NE, _ ne2: NE) -> NE {
  if (Int.random(in: 1..<100) < 50) { return ne1 }
  return ne2
}
@_lifetime(borrow borrow) func testNameConflict(_ borrow: E) -> NE { NE() }
@_lifetime(result: copy source) func testTarget(_ result: inout NE, _ source: consuming NE) { result = source }

actor MyActor {
  nonisolated let constFlag: Bool = false
  nonisolated(unsafe) var mutableFlag: Bool = false
}
func testNonIsolated(actor: MyActor) {
  _ = actor.constFlag
  _ = actor.mutableFlag
}

struct ReferenceOwnershipModifierTest<X: AnyObject> {
    weak var weakValue: X?
    unowned var unownedValue: X
    unowned(safe) var unownedSafeValue: X
    unowned(unsafe) var unmanagedValue: X
}

@_rawLayout(like: T) struct RawStorage<T>: ~Copyable {}
@_rawLayout(like: T, movesAsLike) struct RawStorage2<T>: ~Copyable {}
@_rawLayout(likeArrayOf: T, count: 4) struct RawSmallArray<T>: ~Copyable {}
@_rawLayout(size: 4, alignment: 4) struct Lock: ~Copyable {}

struct LayoutOuter {
  struct Nested<T> {
    var value: T
  }
}
@_rawLayout(like: LayoutOuter.Nested<Int>) struct TypeExprTest: ~Copyable {}

@reasync protocol ReasyncProtocol {}
@rethrows protocol RethrowingProtocol {
  func source() throws
}

@_typeEraser(AnyEraser) protocol EraserProto {}
struct AnyEraser: EraserProto {
  init<T: EraserProto>(erasing: T) {}
}

func takeNone(@_inheritActorContext param: @Sendable () async -> ()) { }
func takeAlways(@_inheritActorContext(always) param: sending @isolated(any) () -> ()) { }
