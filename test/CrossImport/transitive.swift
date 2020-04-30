// This file tests that re-exports can load a cross-import overlay.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDIRECT_FIRST
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDIRECT_SECOND

#if DIRECT_FIRST
import ThinLibrary
import UniversalExports
#elseif DIRECT_SECOND
import UniversalExports
import ThinLibrary
#endif

// We should have loaded the underlying clang module.
fromUniversalExportsClang() // no-error

// We should have loaded core_mi6 too.
fromCoreMI6() // no-error

// We should have loaded _AlwaysImportedOverlay.
fromAlwaysImportedOverlay() // no-error

// We should *not* have loaded _NeverImportedOverlay
fromNeverImportedOverlay() // expected-error {{cannot find 'fromNeverImportedOverlay' in scope}}
