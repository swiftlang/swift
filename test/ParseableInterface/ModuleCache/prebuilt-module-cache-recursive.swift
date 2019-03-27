// RUN: %empty-directory(%t)

// Baseline check: if the module is not in the prebuilt cache, build it
// normally.
// RUN: %empty-directory(%t/prebuilt-cache)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// Do a manual prebuild, and see if it gets picked up.
// RUN: %empty-directory(%t/MCP)
// RUN: sed -e 's/FromInterface/FromPrebuilt/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule - -module-name Lib
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s

// Make sure we installed a forwarding module.
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// What if the module is invalid?
// RUN: rm %t/prebuilt-cache/Lib.swiftmodule && touch %t/prebuilt-cache/Lib.swiftmodule
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs/ -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

import LibExporter

struct X {}
let _: X = Lib.testValue
// FROM-INTERFACE: [[@LINE-1]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-PREBUILT: [[@LINE-2]]:16: error: cannot convert value of type 'FromPrebuilt' to specified type 'X'
