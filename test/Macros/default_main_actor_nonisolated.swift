// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Check for errors
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking -swift-version 6 -default-isolation MainActor

@attached(extension, conformances: Sendable)
macro AddSendable() = #externalMacro(module: "MacroDefinition", type: "SendableMacro")

@AddSendable
final class SendableClass {
  var property: Int { 0 }

  func f() { }
}

nonisolated func acceptSendable<T: Sendable>(_: T) { }

@concurrent func test(sc: SendableClass) async {
  acceptSendable(sc) // SendableClass is Sendable
  acceptSendable(\SendableClass.property) // so is its property
  sc.f() // not on the main actor, so this is okay
}
