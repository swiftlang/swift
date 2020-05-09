// RUN: %empty-directory(%t)

// -- construct prebuilt
// RUN: %empty-directory(%t/prebuilt-cache)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// -- generate a manual prebuilt
// RUN: %empty-directory(%t/MCP)
// RUN: sed -e 's/FromInterface/FromPrebuilt/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | tr -d '\r' | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule - -module-name Lib
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s
// -- ensure we installed a forwarding module
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// -- ensure that the search path is in the root
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk / -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s

// This test is not supported on Windows since the root is not / but rather a
// drive letter followed by a colon.
// UNSUPPORTED: OS=windows-msvc

import Lib

struct X {}
let _: X = Lib.testValue

// FROM-INTERFACE: [[@LINE-2]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-PREBUILT: [[@LINE-3]]:16: error: cannot convert value of type 'FromPrebuilt' to specified type 'X'

