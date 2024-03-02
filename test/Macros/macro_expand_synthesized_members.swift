// REQUIRES: swift_swift_parser, executable_test
//
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5 -verify-ignore-unknown
// RUN: %target-build-swift -g -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(
  member,
  names: named(init), named(Storage), named(storage), named(getStorage()), named(method), named(init(other:))
)
macro addMembers() = #externalMacro(module: "MacroDefinition", type: "AddMembers")

@attached(
  member,
  names: named(`init`), named(Storage), named(storage), named(getStorage()), named(method)
)
macro addMembersQuotedInit() = #externalMacro(module: "MacroDefinition", type: "AddMembers")

#if TEST_DIAGNOSTICS
@addMembers
import Swift
// expected-error@-2 {{'member' macro cannot be attached to import}}
#endif

@addMembers
struct S {
  func useSynthesized() {
    S.method()
    print(type(of: getStorage()))
  }
}

@attached(
  member,
  names: named(extInstanceMethod), named(extStaticMethod)
)
macro addExtMembers() = #externalMacro(module: "MacroDefinition", type: "AddExtMembers")

@addExtMembers
extension S { }

let s = S()

// CHECK: synthesized method
// CHECK: Storage
s.useSynthesized()

// Members added via extension.
s.extInstanceMethod()
S.extStaticMethod()

@attached(member, names: arbitrary)
macro addArbitraryMembers() = #externalMacro(module: "MacroDefinition", type: "AddArbitraryMembers")

@addArbitraryMembers
struct MyType {}

// CHECK: MyType1
// CHECK: MyType2
// CHECK: MyType3
print(MyType.MyType1.self)
print(MyType.MyType2.self)
print(MyType.MyType3.self)

@attached(
  member,
  names: named(RawValue), named(rawValue), named(init)
)
public macro NewType<T>() = #externalMacro(module: "MacroDefinition", type: "NewTypeMacro")

@NewType<String>
public struct MyString {}

// CHECK: String
// CHECK: hello
let myString = MyString("hello")
print(MyString.RawValue.self)
print(myString.rawValue)

struct Base {
  static func member() -> Base { .init() }
}

@attached(member) macro empty(
  _ : Base
) = #externalMacro(module: "MacroDefinition", type: "EmptyMacro")

@empty(.member())
struct TestMacroTypechecking {}

// Macros adding to an enum
@attached(member, names: named(unknown), arbitrary)
public macro ExtendableEnum() = #externalMacro(module: "MacroDefinition", type: "ExtendableEnum")

@ExtendableEnum
enum ElementType {
  case paper
}

print(ElementType.paper.unknown())

#if TEST_DIAGNOSTICS
@addMembersQuotedInit
struct S2 {
// expected-note@-2 {{in expansion of macro 'addMembersQuotedInit' on struct 'S2' here}}
  func useSynthesized() {
    S.method()
    print(type(of: getStorage()))
  }
}
#endif

@attached(
  member,
  names: named(deinit)
)
macro addDeinit() = #externalMacro(module: "MacroDefinition", type: "AddDeinit")

@attached(
  member,
  names: named(subscript(unchecked:))
)
macro addSubscript() = #externalMacro(module: "MacroDefinition", type: "AddSubscript")

@addDeinit
@addSubscript
class C2 {
  init() {
    print("Created a C2")
  }
}

func testC2() {
  // CHECK: Created a C2
  let c2 = C2()
  // CHECK: 17
  print(c2[unchecked: 17])
  // CHECK: deinit was called
}
testC2()

@attached(member, names: arbitrary)
macro GenerateStubs() = #externalMacro(module: "MacroDefinition", type: "GenerateStubMemberMacro")

@freestanding(declaration, names: arbitrary)
macro generateMemberStubs() = #externalMacro(module: "MacroDefinition", type: "GenerateStubsFreestandingMacro")

@freestanding(declaration, names: named(member()))
macro generateMember() = #externalMacro(module: "MacroDefinition", type: "SingleMemberStubMacro")

@GenerateStubs
struct NestedMacroExpansion {}

func callNestedExpansionMember() {
  NestedMacroExpansion.member()
}

@attached(peer, names: prefixed(`__`)) // introduces `__GenerateStubsForProtocolRequirements
@attached(extension, names: arbitrary) // introduces `extension GenerateStubsForProtocolRequirements`
macro GenerateStubsForProtocolRequirements() = #externalMacro(module: "MacroDefinition", type: "GenerateStubsForProtocolRequirementsMacro")

protocol _TestStub {} // used by 'GenerateStubsForProtocolRequirements'

@GenerateStubsForProtocolRequirements
protocol MacroExpansionRequirements {
  func hello(name: String) -> String
}
// struct __MacroExpansionRequirements: _TestStub where ...
// extension MacroExpansionRequirements where Self: _TestStub ...

func testWitnessStub() {
  let stub: any MacroExpansionRequirements = __MacroExpansionRequirements()
  _ = stub.hello(name: "Caplin")
}
