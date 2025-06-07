// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// --- Prepare SDK (.swiftmodule).
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/LibInSDK.framework/Modules/LibInSDK.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name LibInSDK \
// RUN:     -package-name libPkg \
// RUN:     -o %t/SDK/Frameworks/LibInSDK.framework/Modules/LibInSDK.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/Lib.swift

// RUN: test -f %t/SDK/Frameworks/LibInSDK.framework/Modules/LibInSDK.swiftmodule/%module-target-triple.swiftmodule

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown %t/Client1.swift -package-name libPkg -sdk %t/SDK -I %t -I %t/SDK/Frameworks/LibInSDK.framework/Modules

// RUN: %target-swift-frontend -module-name LibLocal -emit-module -emit-module-path %t/LibLocal.swiftmodule -parse-as-library %t/Lib.swift -package-name libPkg
// RUN: test -f %t/LibLocal.swiftmodule

// RUN: %target-swift-frontend -typecheck -verify %t/Client2.swift -package-name libPkg -I %t

// RUN: rm %t/LibLocal.swiftmodule
// RUN: not %target-swift-frontend -typecheck %t/Client2.swift -package-name libPkg -I %t  2>&1 | %FileCheck %s
// CHECK: error: no such module 'LibLocal'

//--- Lib.swift
package func log(level: Int) {}

//--- Client1.swift
import LibInSDK // expected-warning {{module 'LibInSDK' is in package 'libPkg' but was loaded from SDK; modules of the same package should be built locally from source only}}

func someFunc() {
  log(level: 1)
}

//--- Client2.swift
import LibLocal

func someFunc() {
  log(level: 1)
}
