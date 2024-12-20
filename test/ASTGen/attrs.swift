// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature ABIAttribute -enable-experimental-feature Extern -enable-experimental-move-only -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature ABIAttribute -enable-experimental-feature Extern -enable-experimental-move-only > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature ABIAttribute -enable-experimental-feature Extern -enable-experimental-move-only -enable-experimental-feature ParserASTGen

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SymbolLinkageMarkers
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_ABIAttribute

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

@abi(func fn_abi()) // expected-error {{cannot give global function 'fn' the ABI of a global function with a different number of low-level parameters}}
func fn(_: Int) {}

@_alignment(8) struct AnyAlignment {}

@_allowFeatureSuppression(IsolatedAny) public func testFeatureSuppression(fn: @isolated(any) @Sendable () -> ()) {}

@_disallowFeatureSuppression(NoncopyableGenerics) public struct LoudlyNC<T: ~Copyable> {}

@_cdecl("c_function_name") func foo(x: Int) {}

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
