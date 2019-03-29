// RUN: %empty-directory(%t)

// 1) Build a prebuilt cache for our SDK
//
// RUN: mkdir %t/MCP %t/prebuilt-cache %t/my-sdk
// RUN: cp -r %S/Inputs/mock-sdk/. %t/my-sdk
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/ExportedLib.swiftmodule -track-system-dependencies -module-name ExportedLib %t/my-sdk/ExportedLib.swiftinterface
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/SdkLib.swiftmodule -track-system-dependencies -module-name SdkLib %t/my-sdk/SdkLib.swiftinterface
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '1: PASSED'


// 2) Baseline check: Make sure we use the interface when not passing the prebuilt module cache path
//
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SdkLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
//
// Check they are *not* forwarding modules
// RUN: head -n 1 %t/MCP/SdkLib-*.swiftmodule | not grep -e '---'
// RUN: head -n 1 %t/MCP/ExportedLib-*.swiftmodule | not grep -e '---'
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '2: PASSED'


// 3) Baseline check: Make sure we use the the prebuilt module cache when using the SDK it was built with
//
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SdkLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check they *are* forwarding modules
// RUN: head -n 1 %t/MCP/SdkLib-*.swiftmodule | grep -e '---'
// RUN: head -n 1 %t/MCP/ExportedLib-*.swiftmodule | grep -e '---'
//
// Check they contain the expected dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=EXLIB
// RUN: cat %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefixes=EXLIB,SDKLIB
//
// EXLIB: dependencies:
// EXLIB-DAG: /my-sdk/usr/include/module.modulemap
// EXLIB-DAG: /my-sdk/usr/include/SomeCModule.h
// EXLIB-DAG: /my-sdk/ExportedLib.swiftinterface
// SDKLIB-DAG: /my-sdk/SdkLib.swiftinterface
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '3: PASSED'


// 4) Move the SDK without changing its contents
//
// RUN: mv %t/my-sdk %t/my-new-sdk
// RUN: mv %t/prebuilt-cache %t/new-prebuilt-cache
// RUN: %target-swift-frontend -typecheck -I %t/my-new-sdk -sdk %t/my-new-sdk -prebuilt-module-cache-path %t/new-prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SdkLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check they are still both forwarding modules
// RUN: head -n 1 %t/MCP/SdkLib-*.swiftmodule | grep -e '---'
// RUN: head -n 1 %t/MCP/ExportedLib-*.swiftmodule | grep -e '---'
//
// Check they contain the expected dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=NEW-EXLIB
// RUN: cat %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefixes=NEW-EXLIB,NEW-SDKLIB
//
// NEW-EXLIB-DAG: /my-new-sdk/usr/include/module.modulemap
// NEW-EXLIB-DAG: /my-new-sdk/usr/include/SomeCModule.h
// NEW-EXLIB-DAG: /my-new-sdk/ExportedLib.swiftinterface
// NEW-SDKLIB-DAG: /my-new-sdk/SdkLib.swiftinterface
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '4: PASSED'


// 5) Now change the SDK's content and check it no longer uses the prebuilt modules
//
// RUN: echo "// size change" >> %t/my-new-sdk/SdkLib.swiftinterface
// RUN: %target-swift-frontend -typecheck -I %t/my-new-sdk -sdk %t/my-new-sdk -prebuilt-module-cache-path %t/new-prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SDKLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check ExportedLib is still a forwarding module and SdkLib is not
// RUN: head -n 1 %t/MCP/ExportedLib-*.swiftmodule | grep -e '---'
// RUN: head -n 1 %t/MCP/SdkLib-*.swiftmodule | not grep -e '---'
//
// Check ExportedLib still contains the same dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=NEW-EXLIB
//
// RUN: %empty-directory(%t/MCP)
//
// RUN: echo "// size change" >> %t/my-new-sdk/usr/include/SomeCModule.h
// RUN: %target-swift-frontend -typecheck -I %t/my-new-sdk -sdk %t/my-new-sdk -prebuilt-module-cache-path %t/new-prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SDKLib and ExportLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check neither are forwarding modules
// RUN: head -n 1 %t/MCP/SdkLib-*.swiftmodule | not grep -e '---'
// RUN: head -n 1 %t/MCP/ExportedLib-*.swiftmodule | not grep -e '---'
//
// RUN: echo '5: PASSED'

import SdkLib

func foo() -> ExportedInterface {
	return x > 3 ? testValue : testValue2
}
