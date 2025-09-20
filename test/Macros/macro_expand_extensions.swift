// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Check for errors first
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking

// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5 -I %t

// RUN: not %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -serialize-diagnostics-path %t/macro_expand.dia %s -emit-macro-expansion-files no-diagnostics
// RUN: c-index-test -read-diagnostics %t/macro_expand.dia 2>&1 | %FileCheck -check-prefix CHECK-DIAGS %s

// Ensure that we can serialize this file as a module.
// RUN: %target-swift-frontend -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking -emit-module -o %t/MyModule.swiftmodule -enable-testing

// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5 -emit-tbd -emit-tbd-path %t/MacroUser.tbd -I %t
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(extension, conformances: P, names: named(requirement))
macro DelegatedConformance() = #externalMacro(module: "MacroDefinition", type: "DelegatedConformanceViaExtensionMacro")

protocol P {
  static func requirement()
}

struct Wrapped: P {
  static func requirement() {
    print("Wrapped.requirement")
  }
}

@DelegatedConformance
struct Generic<Element> {}

// CHECK-DUMP: @__swiftmacro_23macro_expand_extensions7Generic20DelegatedConformancefMe_.swift
// CHECK-DUMP: extension Generic: P where Element: P {
// CHECK-DUMP:   static func requirement() {
// CHECK-DUMP:     Element.requirement()
// CHECK-DUMP:   }
// CHECK-DUMP: }

func requiresP(_ value: (some P).Type) {
  value.requirement()
}

// CHECK: Wrapped.requirement
requiresP(Generic<Wrapped>.self)

struct Outer {
  @DelegatedConformance
  struct Nested<Element> {}
}

// CHECK-DUMP: @__swiftmacro_23macro_expand_extensions5OuterV6Nested20DelegatedConformancefMe_.swift
// CHECK-DUMP: extension Outer.Nested: P where Element: P {
// CHECK-DUMP:   static func requirement() {
// CHECK-DUMP:     Element.requirement()
// CHECK-DUMP:   }
// CHECK-DUMP: }

// CHECK: Wrapped.requirement
requiresP(Outer.Nested<Wrapped>.self)

@DelegatedConformance
struct AlreadyConforms: P {
  static func requirement() {}
}

// CHECK-DUMP-LABEL: @__swiftmacro_23macro_expand_extensions15AlreadyConforms20DelegatedConformancefMe_.swift
// CHECK-DUMP-NOT: extension AlreadyConforms

@DelegatedConformance
struct AlreadyConformsInExtension {}

extension AlreadyConformsInExtension: P {
  static func requirement() {}
}

// CHECK-DUMP-LABEL: @__swiftmacro_23macro_expand_extensions26AlreadyConformsInExtension20DelegatedConformancefMe_.swift
// CHECK-DUMP-NOT: extension AlreadyConformsInExtension

class ConformsExplicitly<Element>: P {
  static func requirement() {}
}

@DelegatedConformance
class InheritsConformance<Element>: ConformsExplicitly<Element> {}

// CHECK-DUMP-LABEL: @__swiftmacro_23macro_expand_extensions19InheritsConformance09DelegatedE0fMe_.swift
// CHECK-DUMP-NOT: extension InheritsConformance

@DelegatedConformance
class ConformsViaMacro<Element> {
}

// CHECK-DUMP-LABEL: @__swiftmacro_23macro_expand_extensions16ConformsViaMacro20DelegatedConformancefMe_.swift
// CHECK-DUMP: extension ConformsViaMacro: P where Element: P {
// CHECK-DUMP:   static func requirement() {
// CHECK-DUMP:     Element.requirement()
// CHECK-DUMP:   }
// CHECK-DUMP: }

@DelegatedConformance
class InheritsConformanceViaMacro<Element: P>: ConformsViaMacro<Element> {}

// CHECK-DUMP-LABEL: @__swiftmacro_23macro_expand_extensions27InheritsConformanceViaMacro09DelegatedE0fMe_.swift
// CHECK-DUMP-NOT: extension InheritsConformanceViaMacro

#if TEST_DIAGNOSTICS
func testLocal() {
  @DelegatedConformance
  struct Local<Element> {}
  // expected-error@-1{{local type cannot have attached extension macro}}

  struct S {
    @DelegatedConformance
    struct Local<Element> {}
    // expected-error@-1{{local type cannot have attached extension macro}}
  }
}

@DelegatedConformance
typealias A = Int
// expected-error@-2 {{'extension' macro cannot be attached to type alias ('A')}}

@DelegatedConformance
extension Int {}
// expected-error@-2 {{'extension' macro cannot be attached to extension (extension of 'Int')}}

@attached(extension, conformances: P)
macro UndocumentedNamesInExtension() = #externalMacro(module: "MacroDefinition", type: "DelegatedConformanceViaExtensionMacro")

@UndocumentedNamesInExtension
struct S<Element> {}
// expected-note@-2 {{in expansion of macro 'UndocumentedNamesInExtension' on generic struct 'S' here}}

// CHECK-DIAGS: error: declaration name 'requirement()' is not covered by macro 'UndocumentedNamesInExtension'

@attached(extension, names: named(requirement))
macro UndocumentedConformanceInExtension() = #externalMacro(module: "MacroDefinition", type: "AlwaysAddConformance")

@UndocumentedConformanceInExtension
struct InvalidConformance<Element> {}
// expected-note@-2 {{in expansion of macro 'UndocumentedConformanceInExtension' on generic struct 'InvalidConformance' here}}

// CHECK-DIAGS: error: conformance to 'P' is not covered by macro 'UndocumentedConformanceInExtension'

@attached(extension)
macro UndocumentedCodable() = #externalMacro(module: "MacroDefinition", type: "AlwaysAddCodable")

@UndocumentedCodable
struct TestUndocumentedCodable {}
// expected-note@-2 {{in expansion of macro 'UndocumentedCodable' on struct 'TestUndocumentedCodable' here}}

// CHECK-DIAGS: error: conformance to 'Codable' (aka 'Decodable & Encodable') is not covered by macro 'UndocumentedCodable'

@attached(extension, conformances: Decodable)
macro UndocumentedEncodable() = #externalMacro(module: "MacroDefinition", type: "AlwaysAddCodable")

@UndocumentedEncodable
struct TestUndocumentedEncodable {}
// expected-note@-2 {{in expansion of macro 'UndocumentedEncodable' on struct 'TestUndocumentedEncodable' here}}

// CHECK-DIAGS: error: conformance to 'Codable' (aka 'Decodable & Encodable') is not covered by macro 'UndocumentedEncodable'

@attached(extension)
macro BadExtension() = #externalMacro(module: "MacroDefinition", type: "BadExtensionMacro")

// Make sure 'extension Foo' is rejected here as it needs to
// be a qualified reference.
struct HasSomeNestedType {
  @BadExtension // expected-note {{in expansion of macro 'BadExtension' on struct 'SomeNestedType' here}}
  struct SomeNestedType {}
}
// CHECK-DIAGS: error: cannot find type 'SomeNestedType' in scope

#endif

@attached(extension, conformances: Equatable)
macro AvailableEquatable() = #externalMacro(module: "MacroDefinition", type: "ConditionallyAvailableConformance")

@available(macOS 99, *)
@AvailableEquatable
struct TestAvailability {
  static let x : any Equatable.Type = TestAvailability.self
}

protocol P1 {}
protocol P2 {}

@attached(extension, conformances: P1, P2)
macro AddAllConformances() = #externalMacro(module: "MacroDefinition", type: "AddAllConformancesMacro")

@AddAllConformances
struct MultipleConformances {}

// CHECK-DUMP: extension MultipleConformances: P1 {
// CHECK-DUMP: }
// CHECK-DUMP: extension MultipleConformances: P2 {
// CHECK-DUMP: }

@attached(extension, conformances: Equatable, names: named(==))
macro Equatable() = #externalMacro(module: "MacroDefinition", type: "EquatableViaMembersMacro")

@propertyWrapper
struct NotEquatable<T> {
  var wrappedValue: T
}

@Equatable
struct HasPropertyWrappers {
  @NotEquatable
  var value: Int = 0
}

func requiresEquatable<T: Equatable>(_: T) { }
func testHasPropertyWrappers(hpw: HasPropertyWrappers) {
  requiresEquatable(hpw)
}

@Equatable
struct HasPrivateMembers {
  @NotEquatable
  private var value: Int = 0
}

// Check that conformances implied by a macro-defined conformance are serialized
// without issue.
public protocol ImpliesHashable: Hashable { }

@attached(extension, conformances: ImpliesHashable)
macro ImpliesHashable() = #externalMacro(module: "MacroDefinition", type: "ImpliesHashableMacro")

func requiresHashable<T: Hashable>(_: T) { }
func testMakeMeHashable(mmh: MakeMeHashable, dict: [MakeMeHashable: Int]) {
  requiresHashable(mmh)
}

@ImpliesHashable
public struct MakeMeHashable { }


// https://github.com/apple/swift/issues/67989 - member added via extension
// to a protocol is not visible

@attached(extension, names: named(foo), named(printFoo))
public macro FooExtension() = #externalMacro(module: "MacroDefinition", type: "FooExtensionMacro")

@FooExtension
protocol Foo {
  var foo: String { get }
}

extension String: Foo { }

func testStringFoo(s: String) {
  "Test".printFoo()
  s.printFoo()
}

@attached(extension, conformances: Sendable)
macro AddSendable() = #externalMacro(module: "MacroDefinition", type: "SendableMacro")

@AddSendable
final class SendableClass {
}

// expected-warning@+2 {{non-final class 'InvalidSendableClass' cannot conform to the 'Sendable' protocol}}
@AddSendable
class InvalidSendableClass {
}

@AddSendable
struct HasNestedType {
  struct Inner {}
}

// Make sure no circularity error is produced when resolving
// extensions of nested types when the outer type has an
// attached macro that can add other nested types.
extension HasNestedType.Inner {}

@attached(extension, conformances: P, names: named(requirement))
macro AddPWithNonisolated() = #externalMacro(module: "MacroDefinition", type: "PWithNonisolatedFuncMacro")

@attached(extension, conformances: P, names: named(requirement))
macro AddNonisolatedPWithNonisolated() = #externalMacro(module: "MacroDefinition", type: "NonisolatedPWithNonisolatedFuncMacro")

@AddNonisolatedPWithNonisolated
struct MakeMeNonisolated { }

@AddPWithNonisolated
struct KeepMeIsolated { }
