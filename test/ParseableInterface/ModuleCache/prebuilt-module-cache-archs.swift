// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include/Lib.swiftmodule)
// RUN: cp %S/Inputs/prebuilt-module-cache/Lib.swiftinterface %t/include/Lib.swiftmodule/%target-cpu.swiftinterface

// Baseline check: if the prebuilt cache path does not exist, everything should
// still work.
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// Baseline check: if the module is not in the prebuilt cache, build it
// normally.
// RUN: %empty-directory(%t/prebuilt-cache)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// Do a manual prebuild, and see if it gets picked up.
// RUN: %empty-directory(%t/MCP)
// RUN: %empty-directory(%t/prebuilt-cache/Lib.swiftmodule)
// RUN: sed -e 's/FromInterface/FromPrebuilt/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule/%target-swiftmodule-name - -module-name Lib
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s

// Make sure we installed a forwarding module.
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// What if the module is invalid?
// RUN: rm %t/prebuilt-cache/Lib.swiftmodule/%target-swiftmodule-name && touch %t/prebuilt-cache/Lib.swiftmodule/%target-swiftmodule-name
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// What if the arch is missing?
// RUN: rm %t/prebuilt-cache/Lib.swiftmodule/%target-swiftmodule-name
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

import Lib

struct X {}
let _: X = Lib.testValue
// FROM-INTERFACE: [[@LINE-1]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-PREBUILT: [[@LINE-2]]:16: error: cannot convert value of type 'FromPrebuilt' to specified type 'X'
