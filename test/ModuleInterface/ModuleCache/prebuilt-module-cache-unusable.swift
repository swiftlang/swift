// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/MCP)
// RUN: %empty-directory(%t/prebuilt-cache/Lib.swiftmodule)
// RUN: %empty-directory(%t/include/Lib.swiftmodule)
// RUN: cp %S/Inputs/prebuilt-module-cache/Lib.swiftinterface %t/include/Lib.swiftmodule/%target-cpu.swiftinterface

// Prebuild a module for the current target CPU, and put it in the prebuilt cache under some imaginary CPU.
// RUN: sed -e 's/FromInterface/FromPrebuilt/g' %t/include/Lib.swiftmodule/%target-cpu.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule/leg128.swiftmodule - -module-name Lib

// Make sure that, if there's a module for a different architecture
// present in the prebuilt cache, it's ignored and the module is
// rebuilt from an interface.

// RUN: not %target-swift-frontend -typecheck -module-cache-path %t/MCP -sdk %t/include -I %t/include -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck %s --check-prefix FROM-INTERFACE
// RUN: %empty-directory(%t/MCP)

// Make sure it works fine if the module is for this architecture.
// RUN: mv %t/prebuilt-cache/Lib.swiftmodule/leg128.swiftmodule %t/prebuilt-cache/Lib.swiftmodule/%target-swiftmodule-name
// RUN: not %target-swift-frontend -typecheck -module-cache-path %t/MCP -sdk %t/include -I %t/include -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck %s --check-prefix FROM-PREBUILT

// Now make sure it works if there's nothing in the prebuilt cache
// RUN: %empty-directory(%t/prebuilt-cache/Lib.swiftmodule)
// RUN: %empty-directory(%t/MCP)

// RUN: not %target-swift-frontend -typecheck -module-cache-path %t/MCP -sdk %t/include -I %t/include -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck %s --check-prefix FROM-INTERFACE

import Lib

struct X {}
let _: X = Lib.testValue
// FROM-PREBUILT: [[@LINE-1]]:16: error: cannot convert value of type 'FromPrebuilt' to specified type 'X'
// FROM-INTERFACE: [[@LINE-2]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
