// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name swift-utils.log \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Utils.swiftmodule \
// RUN:   -emit-module-interface-path %t/Utils.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Utils.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Utils.swiftinterface) -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/Utils.swiftinterface
// CHECK-PUBLIC-NOT: -package-name swift-utils.log
// CHECK-PUBLIC-NOT: package func packageFunc()
// CHECK-PUBLIC: -module-name Utils
// CHECK-PUBLIC: public func publicFunc()

// RUN: %target-swift-typecheck-module-from-interface(%t/Utils.private.swiftinterface) -module-name Utils -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE < %t/Utils.private.swiftinterface

// CHECK-PRIVATE-NOT: package func packageFunc()
// CHECK-PRIVATE: swift-module-flags-ignorable-private: -package-name swift-utils.log
// CHECK-PRIVATE: public func publicFunc()

// RUN: %target-swift-frontend -typecheck %t/Client.swift -package-name swift-utils.log -I %t -verify

//--- Utils.swift
package func packageFunc() {}
public func publicFunc() {}

//--- Client.swift
import Utils

func clientFunc() {
  packageFunc()
  publicFunc()
}
