// Like prebuilt-module-cache-archs.swift, but testing the fallback behavior.
// This means we have to know the expected names in advance, so this test only
// runs on macOS.

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

// Use the short name "x86_64.swiftmodule".
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include/Lib.swiftmodule)
// RUN: cp %S/Inputs/prebuilt-module-cache/Lib.swiftinterface %t/include/Lib.swiftmodule/x86_64.swiftinterface

// Do a manual prebuild with the long name "x86_64-apple-macos.swiftmodule",
// and see if it gets picked up.
// RUN: %empty-directory(%t/MCP)
// RUN: %empty-directory(%t/prebuilt-cache/Lib.swiftmodule)
// RUN: sed -e 's/FromInterface/FromPrebuiltLong/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule/x86_64-apple-macos.swiftmodule - -module-name Lib
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT-LONG %s
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// Prefer a matching name.
// RUN: %empty-directory(%t/MCP)
// RUN: sed -e 's/FromInterface/FromPrebuiltShort/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/prebuilt-cache/Lib.swiftmodule/x86_64.swiftmodule - -module-name Lib
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT-SHORT %s
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// Prefer a matching name in the other direction too.
// RUN: %empty-directory(%t/MCP)
// RUN: mv %t/include/Lib.swiftmodule/x86_64.swiftinterface %t/include/Lib.swiftmodule/x86_64-apple-macos.swiftinterface
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT-LONG %s
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

// Don't do the fallback thing for long names to short names.
// RUN: %empty-directory(%t/MCP)
// RUN: rm %t/prebuilt-cache/Lib.swiftmodule/x86_64-apple-macos.swiftmodule
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %t/include -I %t/include/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

import Lib

struct X {}
let _: X = Lib.testValue
// FROM-INTERFACE: [[@LINE-1]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-PREBUILT-LONG: [[@LINE-2]]:16: error: cannot convert value of type 'FromPrebuiltLong' to specified type 'X'
// FROM-PREBUILT-SHORT: [[@LINE-3]]:16: error: cannot convert value of type 'FromPrebuiltShort' to specified type 'X'
