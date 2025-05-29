// REQUIRES: swift_swift_parser, executable_test, asserts, concurrency
// REQUIRES: swift_feature_PreambleMacros
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Diagnostics testing
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature PreambleMacros -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS

// Execution testing
// RUN: %target-build-swift -swift-version 5 -g -enable-experimental-feature PreambleMacros -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(body)
macro Remote() = #externalMacro(module: "MacroDefinition", type: "RemoteBodyMacro")

@attached(preamble)
macro Traced() = #externalMacro(module: "MacroDefinition", type: "TracedPreambleMacro")

@attached(preamble, names: named(logger))
macro Logged() = #externalMacro(module: "MacroDefinition", type: "LoggerMacro")

protocol ConjureRemoteValue {
  static func conjureValue() -> Self
}

extension String: ConjureRemoteValue {
  static func conjureValue() -> String { "" }
}

struct Logger {
  func log(entering function: String) {
    print("Logger entering \(function)")
  }

  func log(_ message: String) {
    print("--- \(message)")
  }

  func log(exiting function: String) {
    print("Logger exiting \(function)")
  }
}

func log(_ message: String) {
  print(message)
}

@available(SwiftStdlib 5.1, *)
func remoteCall<Result: ConjureRemoteValue>(function: String, arguments: [String: Any]) async throws -> Result {
  let printedArgs = arguments.keys.sorted().map { key in
    "\(key): \(arguments[key]!)"
  }.joined(separator: ", ")
  print("Remote call \(function)(\(printedArgs))")
  return Result.conjureValue()
}

@available(SwiftStdlib 5.1, *)
@Remote
func f(a: Int, b: String) async throws -> String

@Traced
func doubleTheValue(value: Int) -> Int {
  return value * 2
}

@Logged
func useLogger() {
  let x = 1
  logger.log("use it")
  print(x)
}

@available(SwiftStdlib 5.1, *)
@Remote
@Traced
@Logged
func g(a: Int, b: String) async throws -> String {
  doesNotTypeCheck()
}

#if compiler(>=6.0) && TEST_DIAGNOSTICS
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
  // CHECK: Logger entering g(a: 5, b: World)
  // CHECK: Remote call g(a: 5, b: World)
  // CHECK: Logger exiting g(a:b:)
  // CHECK: Exiting g(a:b:)
  print(try await g(a: 5, b: "World"))
}

// CHECK: Logger entering useLogger()
// CHECK: --- use it
// CHECK: Logger exiting useLogger()
useLogger()
