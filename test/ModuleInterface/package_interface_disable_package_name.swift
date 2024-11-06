// RUN: %empty-directory(%t)

/// By default, -package-name is printed in all interfaces.
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-build-swift \
// RUN:   -emit-module %s -I %t \
// RUN:   -module-name Bar -package-name barpkg \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module-interface-path %t/Bar.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Bar.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Bar.package.swiftinterface

// RUN: %FileCheck %s < %t/Bar.swiftinterface
// RUN: %FileCheck %s < %t/Bar.private.swiftinterface
// RUN: %FileCheck %s < %t/Bar.package.swiftinterface

// CHECK: -enable-library-evolution -package-name barpkg -swift-version 6 -module-name Bar

/// Building modules from non-package interfaces with package-name (default mode) should succeed.
// RUN: %target-swift-frontend -compile-module-from-interface %t/Bar.swiftinterface -o %t/Bar.swiftmodule -module-name Bar
// RUN: rm -rf %t/Bar.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/Bar.private.swiftinterface -o %t/Bar.swiftmodule -module-name Bar
// RUN: rm -rf %t/Bar.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/Bar.package.swiftinterface -o %t/Bar.swiftmodule -module-name Bar

public struct PubStruct {}
@_spi(bar) public struct SPIStruct {}

package struct PkgStruct {}
