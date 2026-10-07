// Outside the "interface" code generation model, clients emit their own
// copies of almost all code, including internal declarations. An explicit
// '@export(interface)' declaration has a unique definition, though, so code
// that clients emit can only refer to it if it's public or
// '@usableFromInline'. That's an error when emitting a TBD file, and a warning
// otherwise.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -verify -verify-additional-prefix warn- -parse-as-library -module-name Lib -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -typecheck %s -verify -verify-additional-prefix warn- -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-swift-frontend -typecheck %s -verify -verify-additional-prefix tbd- -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

@export(interface)
func internalInterface() -> Int { internalHelper() }
// expected-warn-note@-1 2{{global function 'internalInterface()' is not '@usableFromInline' or public}}
// expected-tbd-note@-2 2{{global function 'internalInterface()' is not '@usableFromInline' or public}}

@usableFromInline @export(interface)
func usableFromInlineInterface() -> Int { 2 }

// Clients emit their own copies of other internal declarations.
func internalHelper() -> Int { 1 }

public func generic<T>(_ t: T) -> Int {
  internalInterface()
  // expected-warn-warning@-1 {{global function 'internalInterface()' is internal and cannot be referenced from global function 'generic'}}
  // expected-tbd-error@-2 {{global function 'internalInterface()' is internal and cannot be referenced from global function 'generic'}}
  + usableFromInlineInterface() + internalHelper() + MemoryLayout<T>.size
}

// Non-generic code without the "interface" model is serialized too.
public func nonGeneric() -> Int {
  internalInterface()
  // expected-warn-warning@-1 {{global function 'internalInterface()' is internal and cannot be referenced from global function 'nonGeneric()'}}
  // expected-tbd-error@-2 {{global function 'internalInterface()' is internal and cannot be referenced from global function 'nonGeneric()'}}
}

// The body of an '@export(interface)' declaration isn't emitted into clients.
@export(interface)
public func publicInterface() -> Int { internalInterface() + internalHelper() }
