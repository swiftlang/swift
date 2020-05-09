// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-library -module-name TestModule -module-link-name coreTestModuleKitUtilsTool %S/Inputs/TestModule.swift -emit-module-interface -o %t/%target-library-name(coreTestModuleKitUtilsTool)
// RUN: %target-swift-frontend -emit-ir -I %t -L %t %s | %FileCheck %s

import TestModule

_ = TestStruct()

// CHECK: {{-lcoreTestModuleKitUtilsTool|/DEFAULTLIB:coreTestModuleKitUtilsTool.lib}}
