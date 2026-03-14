// RUN: rm -rf %t
// RUN: mkdir -p %t/System/Library/Frameworks/TestFramework.framework/Modules/TestFramework.swiftmodule
// RUN: mkdir -p %t/Library/Frameworks/TestFramework2.framework/Modules/TestFramework2.swiftmodule
// RUN: %target-build-swift -emit-module -o %t/System/Library/Frameworks/TestFramework.framework/Modules/TestFramework.swiftmodule/%target-swiftmodule-name -module-name TestFramework %s -DFRAMEWORK
// RUN: %target-build-swift -emit-module -o %t/Library/Frameworks/TestFramework2.framework/Modules/TestFramework2.swiftmodule/%target-swiftmodule-name -module-name TestFramework2 %s -DFRAMEWORK

// RUN: not %target-swift-frontend -typecheck -sdk %t -show-diagnostics-after-fatal %s -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=CHECK-%target-runtime %s
// FIXME: This isn't really about objc vs. native runtime,
// but about Apple vs. non-Apple platforms.

#if FRAMEWORK
public func foo() {}
#else

import TestFramework // CHECK-native: error: no such module 'TestFramework'
import TestFramework2 // CHECK-native: error: no such module 'TestFramework2'

TestFramework.foo()
TestFramework2.foo()

// CHECK-objc-NOT: error
// CHECK-objc: error:{{.+}}'dummyError'
// CHECK-objc-NOT: error
dummyError()

#endif // FRAMEWORK
