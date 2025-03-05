// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_ParserASTGen

// RUN: %empty-directory(%t)

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -enable-experimental-feature ParserASTGen

@freestanding(declaration) macro anonymousTypes(_: () -> String) = #externalMacro(module: "MacroDefinition", type: "DefineAnonymousTypesMacro")
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

func foo(a: Int) {
  _ = #stringify(a + 1)
}

struct Outer {
  #anonymousTypes { "test" }
}

@attached(extension, conformances: P1, P2)
macro AddAllConformances() = #externalMacro(module: "MacroDefinition", type: "AddAllConformancesMacro")

protocol P1 {}
protocol P2 {}

@AddAllConformances
struct MultipleConformances {}

func testConformances() {
  func eat(arg: some P1) {}
  eat(arg: MultipleConformances())
}

protocol DefaultInit {
  init()
}

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

@attached(extension, conformances: DefaultInit)
@attached(member, conformances: DefaultInit, names: named(init()), named(f()))
macro DefaultInit() = #externalMacro(module: "MacroDefinition", type: "RequiredDefaultInitMacro")

@DefaultInit
class C { }

@DefaultInit
class D: C { }

@DefaultInit
struct E { }


@attached(memberAttribute)
@attached(member, names: named(_storage))
macro myTypeWrapper() = #externalMacro(module: "MacroDefinition", type: "TypeWrapperMacro")
@attached(accessor)
macro accessViaStorage() = #externalMacro(module: "MacroDefinition", type: "AccessViaStorageMacro")

struct _Storage {
  var x: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
  var y: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
}

@myTypeWrapper
struct S {
  var x: Int
  var y: Int
}


@attached(body)
macro Remote() = #externalMacro(module: "MacroDefinition", type: "RemoteBodyMacro")

protocol ConjureRemoteValue {
  static func conjureValue() -> Self
}
extension String: ConjureRemoteValue {
  static func conjureValue() -> String { "" }
}
func remoteCall<Result: ConjureRemoteValue>(function: String, arguments: [String: Any]) async throws -> Result {
  let printedArgs = arguments.keys.sorted().map { key in
    "\(key): \(arguments[key]!)"
  }.joined(separator: ", ")
  print("Remote call \(function)(\(printedArgs))")
  return Result.conjureValue()
}

@Remote
func f(a: Int, b: String) async throws -> String
