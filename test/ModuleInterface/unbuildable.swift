// Tests the diagnostics emitted when we can't build a module interface, and
// particularly whether or not they call out compiler version mismatches.
//
// First, emit an empty module interface from the current compiler, then add
// some invalid syntax to it:
//
// RUN: %empty-directory(%t)
// RUN: echo "" | %target-swift-frontend -typecheck -emit-module-interface-path %t/UnbuildableCurrent.swiftinterface -enable-library-evolution -swift-version 5 -module-name UnbuildableCurrent -
// RUN: echo "public func fn(_: String = #somethingYouveNeverHeardOf)" >> %t/UnbuildableCurrent.swiftinterface
//
// Next, modify a version of it so it looks like it was built with a different Swift compiler:
//
// RUN: sed -E -e 's|swift-compiler-version: (.*)|swift-compiler-version: NeoTokyoSwift 2000.42|' -e 's|UnbuildableCurrent|UnbuildableFuture|g' %t/UnbuildableCurrent.swiftinterface > %t/UnbuildableFuture.swiftinterface
//
// And finally, test:
//
// RUN: not %target-swift-frontend -typecheck %s -I %t -DCURRENT 2>&1 | %FileCheck -check-prefix=CURRENT %s
// RUN: not %target-swift-frontend -typecheck %s -I %t -DFUTURE 2>&1 | %FileCheck -check-prefix=FUTURE %s

#if CURRENT
import UnbuildableCurrent
// CURRENT: unbuildable.swift:[[@LINE-1]]:8: error: failed to build module 'UnbuildableCurrent' from its module interface; it may have been damaged or it may have triggered a bug in the Swift compiler when it was produced
#else
import UnbuildableFuture
// FUTURE: unbuildable.swift:[[@LINE-1]]:8: error: failed to build module 'UnbuildableFuture' from its module interface; the compiler that produced it, 'NeoTokyoSwift 2000.42', may have used features that aren't supported by this compiler, '{{.*Swift version.*}}'
#endif

