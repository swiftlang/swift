// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -module-name LibBinary -swift-version 5 -I %t \
// RUN:   -package-name libPkg \
// RUN:   -emit-module-path %t/LibBinary.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/ClientLoadBinary.swift -package-name libPkg -I %t -verify

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -module-name LibInterface -swift-version 5 -I %t \
// RUN:   -package-name libPkg \
// RUN:   -enable-library-evolution \
// RUN:   -disable-print-package-name-for-non-package-interface \
// RUN:   -emit-module-path %t/LibBinary.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibInterface.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/LibInterface.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/LibInterface.swiftinterface) -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/LibInterface.swiftinterface
// CHECK-PUBLIC: -module-name LibInterface

// RUN: %target-swift-typecheck-module-from-interface(%t/LibInterface.private.swiftinterface) -module-name LibInterface -I %t -verify

// RUN: %target-swift-frontend -typecheck %t/ClientLoadInterface.swift -package-name otherPkg -I %t -verify

// RUN: %target-swift-frontend -typecheck %t/ClientLoadInterface.swift -package-name libPkg -I %t -verify

//--- Lib.swift
public func publicLog(_ level: Int) {}
package func packageLog(_ level: Int) {}
func internalLog(_ level: Int) {}


//--- ClientLoadInterface.swift
import LibInterface

func someFunc() {
  publicLog(1)
  packageLog(2) // expected-error {{cannot find 'packageLog' in scope}}
}

//--- ClientLoadBinary.swift
import LibBinary

func someFunc() {
  publicLog(1)
  packageLog(2)
}
