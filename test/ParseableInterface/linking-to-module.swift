// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-library -module-name TestModule -module-link-name coreTestModuleKitUtilsTool %S/Inputs/TestModule.swift -emit-parseable-module-interface -o %t/%{target-shared-library-prefix}coreTestModuleKitUtilsTool%{target-shared-library-suffix}
// RUN: %target-swift-frontend -emit-ir -I %t -L %t -enable-parseable-module-interface %s | %FileCheck %s

import TestModule

_ = TestStruct()

// CHECK: -lcoreTestModuleKitUtilsTool
