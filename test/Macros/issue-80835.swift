// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift

// RUN: %target-typecheck-verify-swift -swift-version 6 -load-plugin-library %t/%target-library-name(MacroDefinition) -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 7 -load-plugin-library %t/%target-library-name(MacroDefinition) -verify-additional-prefix swift7-

// REQUIRES: swift_swift_parser
// REQUIRES: swift7

// https://github.com/swiftlang/swift/issues/80835

@available(*, noasync)
func noasyncFn() {}

@_unavailableFromAsync
func unavailableFromAsyncFn() {} // expected-note {{'unavailableFromAsyncFn()' declared here}}

@freestanding(expression)
macro asyncMacro(_ fn: () async -> Void) = #externalMacro(module: "MacroDefinition", type: "GenericToVoidMacro")

@freestanding(declaration)
macro asyncMacroDecl(_ fn: () async -> Void) = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")

@attached(peer)
macro AsyncMacro(_ fn: () async -> Void) = #externalMacro(module: "MacroDefinition", type: "WrapperMacro")

func takesAsyncFn(_ fn: () async -> Void) {}

#asyncMacro {
  defer {
    noasyncFn()
    // expected-swift7-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
    // expected-swift6-warning@-2 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}
  }
  noasyncFn()
  // expected-swift7-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
  // expected-swift6-warning@-2 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}

  unavailableFromAsyncFn()
  // expected-swift7-error@-1 {{global function 'unavailableFromAsyncFn' is unavailable from asynchronous contexts}}
  // expected-swift6-warning@-2 {{global function 'unavailableFromAsyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}

  // This was always an error.
  let _: () async -> Void = {
    noasyncFn()
    // expected-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
  }
}

// This was always an error.
takesAsyncFn {
  noasyncFn()
  // expected-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
}

#asyncMacroDecl {
  noasyncFn()
  // expected-swift7-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
  // expected-swift6-warning@-2 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}
}

typealias Magic<T> = T

// expected-swift7-error@+2 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
// expected-swift6-warning@+1 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}
@AsyncMacro({ noasyncFn() })
func foo() {
  #asyncMacro(({
    noasyncFn()
    // expected-swift7-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
    // expected-swift6-warning@-2 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}
  }))

  #asyncMacroDecl(({
    noasyncFn()
    // expected-swift7-error@-1 {{global function 'noasyncFn' is unavailable from asynchronous contexts}}
    // expected-swift6-warning@-2 {{global function 'noasyncFn' is unavailable from asynchronous contexts; this will be an error in a future Swift language mode}}
  }))

  // This was never treated as async.
  #asyncMacro({
    noasyncFn()
  } as Magic)
}
