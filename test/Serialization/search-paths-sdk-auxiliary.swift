// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/System/Library/Frameworks/TestFramework.framework/Modules/TestFramework.swiftmodule)
// RUN: %empty-directory(%t/System/Library/SubRFrameworksLibrary/Frameworks/TestFramework2.framework/Modules/TestFramework2.swiftmodule)

// RUN: %target-build-swift -emit-module -o %t/System/Library/Frameworks/TestFramework.framework/Modules/TestFramework.swiftmodule/%target-swiftmodule-name -module-name TestFramework %s -DFRAMEWORK
// RUN: %target-build-swift -emit-module -o %t/System/Library/SubFrameworks/TestFramework2.framework/Modules/TestFramework2.swiftmodule/%target-swiftmodule-name -module-name TestFramework2 %s -DFRAMEWORK

// RUN: %target-swift-frontend -typecheck -sdk %t %s -diagnostic-style llvm -Rmodule-loading 2>&1 | %FileCheck %s

#if FRAMEWORK
public func foo() {}
#else

import TestFramework
import TestFramework2

#endif // FRAMEWORK

// CHECK: remark: loaded module 'TestFramework'
// CHECK: remark: loaded module 'TestFramework2'
