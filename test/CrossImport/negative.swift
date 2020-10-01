// This file tests various cases where we do not want to import a cross-import
// overlay, or should not see a particular declaration in it, even though we
// normally would do so.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

//
// The current module is the defining module, bystanding module, or overlay.
// (There's no way the overlay module is ready to load right now.)
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDECLARING_LIBRARY_MISSING -module-name DeclaringLibrary
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DBYSTANDING_LIBRARY_MISSING -module-name BystandingLibrary
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -module-name _OverlayLibrary

//
// The declaring module's import is scoped and that scope excludes
// OverlayLibraryTy.
// (If you import the declaring module scoped, that scope should be applied to
// the overlay. We are still loading the overlay; we just aren't importing the
// declaration in question.)
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDECLARING_LIBRARY_SCOPED

//
// One of the imports is @testable.
// (If you're testing the declaring or bystanding module, you don't want the
// overlay getting in the way. If you're testing the overlay, you know the real
// name to import it by.)
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DBYSTANDING_LIBRARY_TESTABLE -disable-testable-attr-requires-testable-module
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDECLARING_LIBRARY_TESTABLE -disable-testable-attr-requires-testable-module

//
// One of the imports is @_private.
// (If you're using a private import, you're pretending to be part of that
// module.)
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DBYSTANDING_LIBRARY_PRIVATE
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDECLARING_LIBRARY_PRIVATE

//
// One of the modules is not imported.
// (If you don't import one of the required modules, we shouldn' import the
// overlay.)
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DBYSTANDING_LIBRARY_MISSING
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDECLARING_LIBRARY_MISSING

#if BYSTANDING_LIBRARY_TESTABLE
@testable import BystandingLibrary
#elseif BYSTANDING_LIBRARY_PRIVATE
@_private(sourceFile: "Foo.swift") import BystandingLibrary
#elseif BYSTANDING_LIBRARY_MISSING

#else
import BystandingLibrary
#endif

#if DECLARING_LIBRARY_TESTABLE
@testable import DeclaringLibrary
#elseif DECLARING_LIBRARY_PRIVATE
@_private(sourceFile: "Foo.swift") import DeclaringLibrary
#elseif DECLARING_LIBRARY_SCOPED
import struct DeclaringLibrary.DeclaringLibraryTy
#elseif DECLARING_LIBRARY_MISSING

#else
import DeclaringLibrary
#endif

func fn(_: OverlayLibraryTy) {} // expected-error {{cannot find type 'OverlayLibraryTy' in scope}}
