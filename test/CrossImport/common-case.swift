// This explicitly tests "common" cases with well-constructed cross-import
// overlays. Some behaviors here are poorly covered by other tests.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: %{python} %S/Inputs/rewrite-module-triples.py %t %module-target-triple

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -DIMPORT_BYSTANDING_LIBRARY -I %t/include -I %t/lib/swift -F %t/Frameworks
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -import-module BystandingLibrary -I %t/include -I %t/lib/swift -F %t/Frameworks

// Each framework has a cross-import overlay with this library:
#if IMPORT_BYSTANDING_LIBRARY
import BystandingLibrary
#endif

// 1. A Swift framework

import SwiftFramework

fromSwiftFramework()
fromSwiftFrameworkCrossImport()
SwiftFramework.fromSwiftFramework()
SwiftFramework.fromSwiftFrameworkCrossImport()

// 2. A Clang framework

import ClangFramework

fromClangFramework()
fromClangFrameworkCrossImport()
ClangFramework.fromClangFramework()
ClangFramework.fromClangFrameworkCrossImport()

// 3. A Swift-overlaid Clang framework

import OverlaidClangFramework

fromOverlaidClangFramework()
fromOverlaidClangFrameworkOverlay()
fromOverlaidClangFrameworkCrossImport()
OverlaidClangFramework.fromOverlaidClangFramework()
OverlaidClangFramework.fromOverlaidClangFrameworkOverlay()
OverlaidClangFramework.fromOverlaidClangFrameworkCrossImport()
