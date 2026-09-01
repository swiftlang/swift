// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module -verify-ignore-unrelated -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import CFRedeclaredTag

// Equivalent, since CFRedeclaredTagRef should be imported as OpaquePointer
let _: CFRedeclaredTagRef? = CFRedeclaredTagCreate()
let _: OpaquePointer? = CFRedeclaredTagCreate()

// Should not typecheck
let _: Unmanaged<CFRedeclaredTagRef>? = CFRedeclaredTagCreate()
// expected-error@-1 {{'Unmanaged' requires that 'CFRedeclaredTagRef' (aka 'OpaquePointer') be a class type}}
// expected-note@-2 {{requirement specified as 'Instance' : 'AnyObject'}}
