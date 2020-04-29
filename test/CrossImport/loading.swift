// This file tests that we can find the cross-import overlay metadata for every
// kind of module that supports them.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: %{python} %S/Inputs/rewrite-module-triples.py %t %module-target-triple

//
// Meta-tests: These verify that the test harness is functional. If they fail,
// the test is incorrectly constructed.
//

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDIRECTLY_IMPORT_OVERLAYS
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -DTHIN_LIBRARY 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DTHIN_LIBRARY -DNEVER_IMPORTED 2>/dev/null

//
// Actual test cases
//

// Check that we find the overlay in all layouts of Swift and Clang modules,
// and that we generate correct dependencies.
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DTHIN_LIBRARY -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=ThinLibrary -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DFAT_LIBRARY -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=FatLibrary -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DCLANG_LIBRARY -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=ClangLibrary -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DCLANG_LIBRARY_SUBMODULE -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=ClangLibrary -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DSWIFT_FRAMEWORK -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=SwiftFramework -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DCLANG_FRAMEWORK -emit-dependencies-path - | %FileCheck -check-prefixes=DEPS,DEPS-MOST -DMODULE=ClangFramework -DTARGET=%module-target-triple %s
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DOVERLAID_CLANG_FRAMEWORK -emit-dependencies-path - | %FileCheck -check-prefix=DEPS -DMODULE=OverlaidClangFramework -DTARGET=%module-target-triple %s

// Make sure order doesn't matter.
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DALWAYS_IMPORTED_LAST -DTHIN_LIBRARY

// Negative cases (one side of overlay is missing).
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DTHIN_LIBRARY 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DFAT_LIBRARY 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DCLANG_LIBRARY 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DCLANG_LIBRARY_SUBMODULE 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DSWIFT_FRAMEWORK 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DCLANG_FRAMEWORK 2>/dev/null
// RUN: not %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks -DDONT_ALWAYS_IMPORT -DOVERLAID_CLANG_FRAMEWORK 2>/dev/null

//
// Actual test code:
//

// We should have loaded _AlwaysImportedOverlay.
fromAlwaysImportedOverlay() // no-error
fromAlwaysImportedPlatformOverlay() // no-error

// We should *not* have loaded _NeverImportedOverlay
fromNeverImportedOverlay() // expected-error {{cannot find 'fromNeverImportedOverlay' in scope}}

// Unlike real cross-import overlays, our test overlay doesn't @_exported import
// the underlying module, so you can't call underlying declarations at all in
// this test.
//
// The fact that we cannot see these declarations in this test indicates that,
// if we were using a well-formed overlay, the overlay could shadow them if it
// chose.

fromThinLibrary() // expected-error {{cannot find 'fromThinLibrary' in scope}}
fromFatLibrary() // expected-error {{cannot find 'fromFatLibrary' in scope}}
fromClangLibrary() // expected-error {{cannot find 'fromClangLibrary' in scope}}
fromSwiftFramework() // expected-error {{cannot find 'fromSwiftFramework' in scope}}
fromClangFramework() // expected-error {{cannot find 'fromClangFramework' in scope}}
fromOverlaidClangFramework() // expected-error {{cannot find 'fromOverlaidClangFramework' in scope}}

//
// FileCheck output for -emit-dependencies:
//

// DEPS-LABEL: - :

// DEPS-NOT: lib{{/|\\}}swift{{/|\\}}_NeverImportedOverlay.swiftinterface
// DEPS-NOT: badarch-badvendor-bados

// DEPS-DAG: lib{{/|\\}}swift{{/|\\}}_AlwaysImportedOverlay.swiftinterface
// DEPS-DAG: lib{{/|\\}}swift{{/|\\}}_AlwaysImportedPlatformOverlay.swiftinterface
// DEPS-DAG: [[MODULE]].swiftcrossimport{{/|\\}}AlwaysImported.swiftoverlay

// OverlaidClangImporter does not expect this line; it uses
// _AlwaysImportedPlatformOverlay to make sure that the underlying clang
// module's cross-imports are honored.
// DEPS-MOST-DAG: [[MODULE]].swiftcrossimport{{/|\\}}[[TARGET]]{{/|\\}}AlwaysImported.swiftoverlay

// FIXME: Better if we didn't?
// DEPS-DAG: [[MODULE]].swiftcrossimport{{/|\\}}NeverImported.swiftoverlay

//
// Imports used in certain test cases:
//

#if !ALWAYS_IMPORTED_LAST && !DONT_ALWAYS_IMPORT
import AlwaysImported
#endif

fromAlwaysImported()

#if THIN_LIBRARY
import ThinLibrary
#endif

#if FAT_LIBRARY
import FatLibrary
#endif

#if CLANG_LIBRARY
import ClangLibrary
#endif

#if CLANG_LIBRARY_SUBMODULE
import ClangLibrary.Submodule
#endif

#if SWIFT_FRAMEWORK
import SwiftFramework
#endif

#if CLANG_FRAMEWORK
import ClangFramework
#endif

#if OVERLAID_CLANG_FRAMEWORK
import OverlaidClangFramework
#endif

#if NEVER_IMPORTED
import NeverImported
#endif

#if DIRECTLY_IMPORT_OVERLAYS
import _AlwaysImportedOverlay
import _AlwaysImportedPlatformOverlay
#endif

#if ALWAYS_IMPORTED_LAST
import AlwaysImported
#endif
