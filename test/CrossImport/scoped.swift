// This file tests that scoped imports work as expected with cross-import
// overlays.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DBYSTANDING_SCOPED


// Scope of bystanding library import shouldn't matter.
#if BYSTANDING_SCOPED
import struct BystandingLibrary.BystandingLibraryTy
#else
import BystandingLibrary
#endif

import struct DeclaringLibrary.OverlayLibraryTy
import struct DeclaringLibrary.DeclaringLibraryTy

func fn1(_: OverlayLibraryTy) {} // no-error
func fn2(_: DeclaringLibraryTy) {} // no-error
func fn3(_: ShadowTy) {} // expected-error {{cannot find type 'ShadowTy' in scope}}
