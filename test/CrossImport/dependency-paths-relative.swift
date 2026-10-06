// This file tests that a cross-import overlay is recorded as an incremental
// dependency relative to the module search path, rather than as an absolute
// path, when the search path is itself relative.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: cd %t && %target-swift-frontend -compile-module-from-interface lib/swift/DeclaringLibrary.swiftinterface -o lib/swift/DeclaringLibrary.swiftmodule -module-name DeclaringLibrary
// RUN: cd %t && %target-swift-frontend -compile-module-from-interface lib/swift/BystandingLibrary.swiftinterface -o lib/swift/BystandingLibrary.swiftmodule -module-name BystandingLibrary

// Compile from within %t with working-directory-relative explicit module paths
// so the scanned `.swiftcrossimport` directory is itself relative, and capture
// the incremental dependencies.
// RUN: cd %t && %target-swift-frontend -typecheck -primary-file %s -enable-cross-import-overlays -I include -I lib/swift -F Frameworks -swift-module-file=DeclaringLibrary=lib/swift/DeclaringLibrary.swiftmodule -swift-module-file=BystandingLibrary=lib/swift/BystandingLibrary.swiftmodule -emit-reference-dependencies-path %t/deps.swiftdeps
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/deps.swiftdeps | %FileCheck %s

import DeclaringLibrary
import BystandingLibrary

// The overlay path must remain relative when the explicit module path is
// relative, without also recording an absolute path.
// CHECK-NOT: externalDepend interface '' '{{.*}}{{/|\\}}lib{{/|\\}}swift{{/|\\}}DeclaringLibrary.swiftcrossimport{{/|\\}}BystandingLibrary.swiftoverlay' false
// CHECK: externalDepend interface '' 'lib{{/|\\}}swift{{/|\\}}DeclaringLibrary.swiftcrossimport{{/|\\}}BystandingLibrary.swiftoverlay' false
// CHECK-NOT: externalDepend interface '' '{{.*}}{{/|\\}}lib{{/|\\}}swift{{/|\\}}DeclaringLibrary.swiftcrossimport{{/|\\}}BystandingLibrary.swiftoverlay' false
