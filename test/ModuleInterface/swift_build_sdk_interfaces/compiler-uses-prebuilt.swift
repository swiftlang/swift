// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/sdk/usr/lib/swift/Normal.swiftmodule
// RUN: mkdir -p %t/sdk/System/Library/Frameworks/FMWK.framework/Modules/FMWK.swiftmodule

// RUN: echo 'public func normal() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/usr/lib/swift/Normal.swiftmodule/%target-cpu.swiftinterface -emit-module -o /dev/null -module-name Normal
// RUN: echo 'public func flat() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/usr/lib/swift/Flat.swiftinterface -emit-module -o /dev/null -module-name Flat
// RUN: echo 'public func fmwk() {}' | %target-swift-frontend - -emit-module-interface-path %t/sdk/System/Library/Frameworks/FMWK.framework/Modules/FMWK.swiftmodule/%target-cpu.swiftinterface -emit-module -o /dev/null -module-name FMWK

// RUN: %swift_build_sdk_interfaces -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -v -o %t/prebuilt
// RUN: ls %t/prebuilt | %FileCheck %s
// CHECK-DAG: Normal.swiftmodule
// CHECK-DAG: Flat.swiftmodule
// CHECK-DAG: FMWK.swiftmodule

// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/*.swiftmodule

// Touch a file in the SDK (to make it look like it changed) and try again.
// This should still be able to use the prebuilt modules because they track
// content hashes, not just size+mtime.
// RUN: rm -rf %t/MCP
// RUN: %{python} %S/../ModuleCache/Inputs/make-old.py %t/sdk/usr/lib/swift/Normal.swiftmodule/%target-cpu.swiftinterface
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: ls %t/MCP/*.swiftmodule | %FileCheck -check-prefix CHECK-CACHE %s
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/*.swiftmodule

// CHECK-CACHE-DAG: Normal-{{.+}}.swiftmodule
// CHECK-CACHE-DAG: Flat-{{.+}}.swiftmodule
// CHECK-CACHE-DAG: FMWK-{{.+}}.swiftmodule

// Actually change a file in the SDK, to check that we're tracking dependencies
// at all.
// RUN: rm -rf %t/MCP
// RUN: echo "public func another()" >> %t/sdk/usr/lib/swift/Normal.swiftmodule/%target-cpu.swiftinterface
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -module-cache-path %t/MCP -prebuilt-module-cache-path %t/prebuilt
// RUN: ls %t/MCP/*.swiftmodule | %FileCheck -check-prefix CHECK-CACHE %s
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Normal-*.swiftmodule
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Flat-*.swiftmodule
// RUN: %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/FMWK-*.swiftmodule

// Without the prebuilt cache everything should still work; it'll just take time
// because we have to build the interfaces.
// RUN: rm -rf %t/MCP
// RUN: %target-typecheck-verify-swift -sdk %t/sdk -Fsystem %t/sdk/System/Library/Frameworks -I %t/sdk/usr/lib/swift/ -module-cache-path %t/MCP
// RUN: ls %t/MCP/*.swiftmodule | %FileCheck -check-prefix CHECK-CACHE %s
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Normal-*.swiftmodule
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/Flat-*.swiftmodule
// RUN: not %{python} %S/../ModuleCache/Inputs/check-is-forwarding-module.py %t/MCP/FMWK-*.swiftmodule


import Normal
import Flat
import FMWK

func test() {
  normal()
  flat()
  fmwk()
}
