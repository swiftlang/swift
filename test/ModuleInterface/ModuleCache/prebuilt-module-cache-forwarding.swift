// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/MCP)
// RUN: %empty-directory(%t/prebuilt-cache)

// First, prebuild a module and put it in the prebuilt cache.
// RUN: sed -e 's/FromInterface/FromPrebuilt/g' %S/Inputs/prebuilt-module-cache/Lib.swiftinterface | tr -d '\r' > %t/Lib.swiftinterface.tmp
// RUN: mv %t/Lib.swiftinterface.tmp %t/Lib.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface -module-cache-path %t/MCP -serialize-parseable-module-interface-dependency-hashes -o %t/prebuilt-cache/Lib.swiftmodule %t/Lib.swiftinterface

// Next, use the module and check if the forwarding module is in place.
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s

// Make sure we installed a forwarding module.
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule
// RUN: cat %t/MCP/Lib-*.swiftmodule

// Now invalidate a dependency of the prebuilt module, by changing the hash of the .swiftinterface.
// RUN: cp %t/Lib.swiftinterface %t/Lib.swiftinterface.moved-aside
// RUN: echo ' ' >> %t/Lib.swiftinterface

// Make sure the forwarding file is replaced with a real module.
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// Delete the cached module we just created, put the old .swiftinterface back, and create the forwarding module again
// RUN: %empty-directory(%t/MCP)
// RUN: mv %t/Lib.swiftinterface.moved-aside %t/Lib.swiftinterface
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s

// Move the prebuilt module out of the way, so the forwarding module points to nothing.
// RUN: mv %t/prebuilt-cache/Lib.swiftmodule %t/prebuilt-cache/NotLib.swiftmodule

// Make sure we delete the existing forwarding module and rebuild from an interface.
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// Move the prebuilt module back to its original path
// RUN: mv %t/prebuilt-cache/NotLib.swiftmodule %t/prebuilt-cache/Lib.swiftmodule

// If the forwarding module is corrupted, we shouldn't rebuild the module in the cache,
// we should delete it and generate a new forwarding module.
// RUN: %empty-directory(%t/MCP)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule
// RUN: %{python} %S/../Inputs/make-unreadable.py %t/MCP/Lib-*.swiftmodule
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache/ -prebuilt-module-cache-path %t/prebuilt-cache %s 2>&1 | %FileCheck -check-prefix=FROM-PREBUILT %s
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/Lib-*.swiftmodule

import Lib

struct X {}
let _: X = Lib.testValue
// FROM-INTERFACE: [[@LINE-1]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-PREBUILT: [[@LINE-2]]:16: error: cannot convert value of type 'FromPrebuilt' to specified type 'X'
