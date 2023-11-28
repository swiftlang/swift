// REQUIRES: swift_swift_parser, executable_test, asserts, concurrency
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Diagnostics testing
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature BodyMacros -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS

// Execution testing
// RUN: %target-build-swift -swift-version 5 -g -enable-experimental-feature BodyMacros -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(body)
macro Remote() = #externalMacro(module: "MacroDefinition", type: "RemoteBodyMacro")

@attached(preamble)
macro Traced() = #externalMacro(module: "MacroDefinition", type: "TracedPreambleMacro")

@attached(preamble)
macro Log2() = #externalMacro(module: "MacroDefinition", type: "Log2PreambleMacro")

protocol ConjureRemoteValue {
  static func conjureValue() -> Self
}

extension String: ConjureRemoteValue {
  static func conjureValue() -> String { "" }
}

@available(SwiftStdlib 5.1, *)
func remoteCall<Result: ConjureRemoteValue>(function: String, arguments: [String: Any]) async throws -> Result {
  let printedArgs = arguments.keys.sorted().map { key in 
    "\(key): \(arguments[key]!)"
  }.joined(separator: ", ")
  print("Remote call \(function)(\(printedArgs))")
  return Result.conjureValue()
}

func log(_ value: String) {
  print(value)
}

func log2(_ value: String) {
  print("log2(\(value))")
}

@Traced
func doubleTheValue(value: Int) -> Int {
  return value * 2
}

@available(SwiftStdlib 5.1, *)
@Remote
func f(a: Int, b: String) async throws -> String

@available(SwiftStdlib 5.1, *)
@Remote
@Traced
@Log2
func g(a: Int, b: String) async throws -> String {
  doesNotTypeCheck()
}

#if compiler(>=5.11) && TEST_DIAGNOSTICS
@available(SwiftStdlib 5.1, *)
@Remote
func h(a: Int, b: String) async throws -> String {
  does not
  // expected-error@-1{{consecutive statements on a line must be separated by ';'}}
  parse
}
#endif

// CHECK: Entering doubleTheValue(value: 7)
// CHECK-NEXT: Exiting doubleTheValue(value:)
_ = doubleTheValue(value: 7)

if #available(SwiftStdlib 5.1, *) {
  // CHECK: Remote call f(a: 5, b: Hello)
  print(try await f(a: 5, b: "Hello"))

  // CHECK: Entering g(a: 5, b: World)
  // CHECK: log2(Entering g(a: 5, b: World))
  // CHECK: Remote call g(a: 5, b: World)
  // CHECK: log2(Exiting g(a:b:))
  // CHECK: Exiting g(a:b:)
  print(try await g(a: 5, b: "World"))
}
