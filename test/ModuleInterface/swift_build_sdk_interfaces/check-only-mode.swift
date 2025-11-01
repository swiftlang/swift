// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/sdk/System/iOSSupport/usr/lib/swift/Normal.swiftmodule
// RUN: mkdir -p %t/sdk/System/Library/Frameworks/FMWK.framework/Modules/FMWK.swiftmodule
// RUN: %empty-directory(%t/sdk/usr/lib/swift/%target-sdk-name)
// RUN: cp -r %platform-module-dir/{_Concurrency,_StringProcessing,Swift,SwiftOnoneSupport}.swiftmodule %t/sdk/usr/lib/swift/%target-sdk-name/
// RUN: cp -r %test-resource-dir/shims %t/sdk/usr/lib/swift

// RUN: echo 'public func normal() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/System/iOSSupport/usr/lib/swift/Normal.swiftmodule/%target-swiftinterface-name -emit-module -o /dev/null -module-name Normal
// RUN: echo 'public func flat() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/usr/lib/swift/Flat.swiftinterface -emit-module -o /dev/null -module-name Flat
// RUN: echo 'public func fmwk() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/System/Library/Frameworks/FMWK.framework/Modules/FMWK.swiftmodule/%target-swiftinterface-name -emit-module -o /dev/null -module-name FMWK

// RUN: %swift_build_sdk_interfaces -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -v -o %t/prebuilt -check-only
// RUN: ls %t/prebuilt | %FileCheck %s
// CHECK-DAG: Normal.swiftmodule
// CHECK-DAG: Flat.swiftmodule
// CHECK-DAG: FMWK.swiftmodule

// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -I %t/sdk/System/iOSSupport/usr/lib/swift -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/FMWK-*.swiftmodule
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Flat-*.swiftmodule
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Normal-*.swiftmodule

// Touch a file in the SDK (to make it look like it changed) and try again.
// In -check-only mode, this should force a rebuild.
// RUN: rm -rf %t/MCP
// RUN: %{python} %S/../ModuleCache/Inputs/make-old.py %t/sdk/System/iOSSupport/usr/lib/swift/Normal.swiftmodule/%target-swiftinterface-name
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -I %t/sdk/System/iOSSupport/usr/lib/swift -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Normal-*.swiftmodule

import Normal
import Flat
import FMWK

func test() {
  normal()
  flat()
  fmwk()
}
