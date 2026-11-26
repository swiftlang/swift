// REQUIRES: swift_swift_parser
//
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition)

@attached(
  member,
  names: named(init), named(Storage), named(storage), named(getStorage()), named(method), named(init(other:))
)
macro addMembers() = #externalMacro(module: "MacroDefinition", type: "AddMembers")

@attached(member, names: named(RawValue), named(rawValue), named(init))
macro NewType<T>() = #externalMacro(module: "MacroDefinition", type: "NewTypeMacro")

@attached(extension, conformances: Equatable)
macro Equatable() = #externalMacro(module: "MacroDefinition", type: "EquatableMacro")

@freestanding(declaration, names: arbitrary)
macro bitwidthNumberedStructs<T>(_ baseName: String, _: T.Type) = #externalMacro(module: "MacroDefinition", type: "DefineBitwidthNumberedStructsMacro")

// Both extensions can only be bound once macros are expanded.
@NewType<Int>
struct S1: A.Storage.P {}
extension S1.RawValue {}

// An extension macro adds another conformance.
@Equatable @NewType<Int>
struct S2: A.Storage.P {}
extension S2.RawValue {}

// Type-checking the macro argument looks into the enclosing type.
struct S3: A.Storage.P { #bitwidthNumberedStructs("Inner", Int.self) }
extension S3.Inner8 {}

// Substituting the typealias from the protocol extension looks up a
// conformance.
protocol R {}
extension R { typealias U = Int }

struct S4: R, A.Storage.P { #bitwidthNumberedStructs("Inner", S4.U.self) }
extension S4.Inner8 {}

@addMembers
struct A {}
extension A.Storage { protocol P {} }

func takesP(_: any A.Storage.P) {}
func takesEquatable(_: some Equatable) {}
func testConformance() {
  takesP(S1(0))
  takesP(S2(0))
  takesEquatable(S2(0))
  takesP(S3())
  takesP(S4())
}
