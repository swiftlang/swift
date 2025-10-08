// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/sdk/System/iOSSupport/usr
// RUN: %empty-directory(%t/sdk/usr/lib/swift/%target-sdk-name)
// RUN: cp -r %S/Inputs/system-dependencies-sdk/usr/lib %t/sdk/System/iOSSupport/usr
// RUN: cp -r %S/Inputs/system-dependencies-sdk/usr/include %t/sdk/usr
// RUN: cp -r %platform-module-dir/{_Concurrency,_StringProcessing,Swift,SwiftOnoneSupport}.swiftmodule %t/sdk/usr/lib/swift/%target-sdk-name/
// RUN: cp -r %test-resource-dir/shims %t/sdk/usr/lib/swift
// RUN: echo 'import Platform; public func usesCStruct(_: SomeCStruct?) {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/System/iOSSupport/usr/lib/swift/Swifty.swiftmodule/%target-swiftinterface-name -emit-module -o /dev/null -module-name Swifty -sdk %t/sdk

// RUN: %swift_build_sdk_interfaces -sdk %t/sdk -v -o %t/prebuilt
// RUN: ls %t/prebuilt | %FileCheck %s
// CHECK: Swifty.swiftmodule

// RUN: %target-typecheck-verify-swift -sdk %t/sdk -I %t/sdk/usr/lib/swift/ -I %t/sdk/System/iOSSupport/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Swifty-*.swiftmodule

// Touch a file in the SDK (to make it look like it changed) and try again.
// This should still be able to use the prebuilt modules because they track
// content hashes, not just size+mtime.
// RUN: rm -rf %t/MCP
// RUN: %{python} %S/../ModuleCache/Inputs/make-old.py %t/sdk/usr/include/Platform.h
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -I %t/sdk/usr/lib/swift/ -I %t/sdk/System/iOSSupport/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Swifty-*.swiftmodule

// Actually change a file in the SDK, to check that we're not tracking system dependencies
// at all.
// RUN: rm -rf %t/MCP
// RUN: echo "void unrelated();" >> %t/sdk/usr/include/Platform.h
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -I %t/sdk/usr/lib/swift/ -I %t/sdk/System/iOSSupport/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Swifty-*.swiftmodule

// Without the prebuilt cache everything should still work; it'll just take time
// because we have to build the interfaces.
// RUN: rm -rf %t/MCP
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -I %t/sdk/System/iOSSupport/usr/lib/swift/ -module-cache-path %t/MCP
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Swifty-*.swiftmodule

import Swifty

usesCStruct(nil)
