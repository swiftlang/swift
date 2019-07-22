// RUN: %empty-directory(%t)

// 1) Build a prebuilt cache for our SDK
//
// RUN: mkdir %t/MCP %t/prebuilt-cache %t/my-sdk
// RUN: cp -r %S/Inputs/mock-sdk/. %t/my-sdk
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/ExportedLib.swiftmodule -track-system-dependencies -module-name ExportedLib %t/my-sdk/ExportedLib.swiftinterface
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/SdkLib.swiftmodule -track-system-dependencies -module-name SdkLib %t/my-sdk/SdkLib.swiftinterface
//
// Check the prebuilt modules don't contain dependencies in the module cache, prebuilt cache, or resource dir
// RUN: llvm-bcanalyzer -dump %t/prebuilt-cache/ExportedLib.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
// RUN: llvm-bcanalyzer -dump %t/prebuilt-cache/SdkLib.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
//
// PREBUILT: MODULE_BLOCK
// PREBUILT-NOT: FILE_DEPENDENCY {{.*[/\\]MCP[/\\]}}
// PREBUILT-NOT: FILE_DEPENDENCY {{.*[/\\]prebuilt-cache[/\\]}}
// PREBUILD-NOT: FILE_DEPENDENCY {{.*[/\\]lib[/\\]swift[/\\]}}
//
// Re-build them in the opposite order
// RUN: %empty-directory(%t/prebuilt-cache)
// RUN: %empty-directory(%t/MCP)
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/SdkLib.swiftmodule -track-system-dependencies -module-name SdkLib %t/my-sdk/SdkLib.swiftinterface
// RUN: %target-swift-frontend -build-module-from-parseable-interface -serialize-parseable-module-interface-dependency-hashes -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -I %t/my-sdk -module-cache-path %t/MCP -o %t/prebuilt-cache/ExportedLib.swiftmodule -track-system-dependencies -module-name ExportedLib %t/my-sdk/ExportedLib.swiftinterface
//
// Check they still don't contain dependencies in the module cache or prebuilt cache
// RUN: llvm-bcanalyzer -dump %t/prebuilt-cache/ExportedLib.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
// RUN: llvm-bcanalyzer -dump %t/prebuilt-cache/SdkLib.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '1: PASSED'


// 2) Baseline check: Make sure we use the interface when not passing the prebuilt module cache path
//
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies -disable-modules-validate-system-headers %s
//
// Check SdkLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
//
// Check they are *not* forwarding modules
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
//
// Check they don't contain dependencies in the module cache (..or prebuilt cache)
// RUN: llvm-bcanalyzer -dump %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
// RUN: llvm-bcanalyzer -dump %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
//
// Check we didn't emit anything from the cache in the .d file either
// RUN: cat %t/dummy.d | %FileCheck %s -check-prefix=DEPFILE-NEGATIVE
// RUN: cat %t/dummy.d | %FileCheck %s -check-prefix=DEPFILE
//
// DEPFILE-NEGATIVE-NOT: {{[/\\]MCP[/\\]}}
// DEPFILE-NEGATIVE-NOT: {{[/\\]prebuilt-cache[/\\]}}
//
// DEPFILE-DAG: SomeCModule.h
// DEPFILE-DAG: SdkLib.swiftinterface
// DEPFILE-DAG: ExportedLib.swiftinterface
// DEPFILE-DAG: SDKDependencies-disable-validation.swift
//
// RUN: %empty-directory(%t/MCP)
// RUN: echo '2: PASSED'


// 3) Baseline check: Make sure we use the the prebuilt module cache when using the SDK it was built with
//
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies -disable-modules-validate-system-headers %s
//
// Check SdkLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check they *are* forwarding modules
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
//
// Check they contain the expected dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=EXLIB
// RUN: cat %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefixes=EXLIB,SDKLIB
//
// EXLIB: dependencies:
// EXLIB-DAG: path: '{{usr[/\\]include[/\\]}}module.modulemap'
// EXLIB-DAG: path: '{{usr[/\\]include[/\\]}}SomeCModule.h'
// EXLIB-DAG: path: ExportedLib.swiftinterface
// SDKLIB-DAG: SdkLib.swiftinterface
//
// Check they don't contain any dependencies from either cache other than themselves
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=NOCACHE -DLIB_NAME=ExportedLib
// RUN: cat %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefix=NOCACHE -DLIB_NAME=SdkLib
//
// NOCACHE: dependencies:
// NOCACHE-NOT: {{[/\\]prebuilt-cache[/\\]}}
// NOCACHE-NOT: {{[/\\]MCP[/\\]}}
// NOCACHE: {{[/\\]prebuilt-cache[/\\]}}[[LIB_NAME]].swiftmodule
// NOCACHE-NOT: {{[/\\]prebuilt-cache[/\\]}}
// NOCACHE-NOT: {{[/\\]MCP[/\\]}}
//
// Check we didn't emit anything from the cache in the .d file either
// RUN: cat %t/dummy.d | %FileCheck %s -check-prefix=DEPFILE-NEGATIVE
// RUN: cat %t/dummy.d | %FileCheck %s -check-prefix=DEPFILE
//
// RUN: echo '3: PASSED'


// 4) Now change the SDK's content WITHOUT clearing the module cache and check
// that it doesn't change anything...
//
// RUN: echo "// size change" >> %t/my-sdk/SdkLib.swiftinterface
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies -disable-modules-validate-system-headers %s
//
// Check SDKLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check that they're both still forwarding modules, because we didn't stat
// the system dependencies.
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
//
// Check ExportedLib still contains the same dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=EXLIB
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=NOCACHE -DLIB_NAME=ExportedLib
//
// RUN: echo '4: PASSED'

// 5) ...but if we clear the module cache and start over, we won't be using the
// prebuilt module anymore for SDKLib.
//
// RUN: %empty-directory(%t/MCP)
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies -disable-modules-validate-system-headers %s
//
// Check SDKLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check ExportedLib is still a forwarding module and SdkLib is not
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
//
// Check ExportedLib still contains the same dependencies
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=EXLIB
// RUN: cat %t/MCP/ExportedLib-*.swiftmodule | %FileCheck %s -check-prefix=NOCACHE -DLIB_NAME=ExportedLib
//
// Check SdkLib doesn't contain dependencies in the module cache or prebuilt cache
// RUN: llvm-bcanalyzer -dump %t/MCP/SdkLib-*.swiftmodule | %FileCheck %s -check-prefix=PREBUILT
//
// RUN: echo '5: PASSED'

// 6) If we do one more change, we won't rebuild SdkLib *again*...
//
// RUN: %{python} %S/Inputs/make-old.py %t/MCP/SdkLib-*.swiftmodule
// RUN: echo "// size change" >> %t/my-sdk/SdkLib.swiftinterface
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies -disable-modules-validate-system-headers %s
//
// Check SDKLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check ExportedLib is still a forwarding module and SdkLib is not
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
//
// Check SdkLib hasn't been rebuilt.
// RUN: %{python} %S/Inputs/check-is-old.py %t/MCP/SdkLib-*.swiftmodule
//
// RUN: echo '6: PASSED'

// 7) ...until we turn off -disable-modules-validate-system-headers.
//
// RUN: %{python} %S/Inputs/make-old.py %t/MCP/SdkLib-*.swiftmodule
// RUN: echo "// size change" >> %t/my-sdk/SdkLib.swiftinterface
// RUN: %target-swift-frontend -typecheck -I %t/my-sdk -sdk %t/my-sdk -prebuilt-module-cache-path %t/prebuilt-cache -module-cache-path %t/MCP -emit-dependencies-path %t/dummy.d -track-system-dependencies %s
//
// Check SDKLib and ExportedLib are in the module cache
// RUN: test -f %t/MCP/SdkLib-*.swiftmodule
// RUN: test -f %t/MCP/ExportedLib-*.swiftmodule
//
// Check ExportedLib is still a forwarding module and SdkLib is not
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/ExportedLib-*.swiftmodule
// RUN: not %{python} %S/Inputs/check-is-forwarding-module.py %t/MCP/SdkLib-*.swiftmodule
//
// Check SdkLib has been rebuilt.
// RUN: not %{python} %S/Inputs/check-is-old.py %t/MCP/SdkLib-*.swiftmodule
//
// RUN: echo '7: PASSED'


import SdkLib

func foo() -> ExportedInterface {
	return x > 3 ? testValue : testValue2
}
