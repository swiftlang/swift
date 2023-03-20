// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name LibFromInterface -emit-module -emit-module-interface-path %t/LibFromInterface.swiftinterface -parse-as-library %t/Lib.swift -enable-library-evolution -package-name mypkg -swift-version 5
// RUN: test -f %t/LibFromInterface.swiftinterface
// RUN: %FileCheck %s -check-prefix CHECK-LIB < %t/LibFromInterface.swiftinterface
// CHECK-LIB: -package-name mypkg
// CHECK-LIB-NOT: func log(level: Int)

// RUN: not %target-swift-frontend -module-name ClientInSamePkg %t/ClientLoadInterfaceModule.swift -emit-module -emit-module-path %t/ClientInSamePkg.swiftmodule -package-name mypkg -I %t 2> %t/resultA.output
// RUN: %FileCheck %s -check-prefix CHECK-A < %t/resultA.output
// CHECK-A: error: module 'LibFromInterface' is in package 'mypkg' but was built from interface '{{.*}}LibFromInterface.swiftinterface'; modules of the same package can only be loaded if built from source

// RUN: not %target-swift-frontend -module-name ClientInDiffPkg %t/ClientLoadInterfaceModule.swift -emit-module -emit-module-path %t/ClientInDiffPkg.swiftmodule -package-name otherPkg -I %t 2> %t/resultB.output
// RUN: %FileCheck %s -check-prefix CHECK-B < %t/resultB.output
// CHECK-B: error: cannot find 'log' in scope

// RUN: %target-swift-frontend -module-name LibFromSource -emit-module -emit-module-path %t/LibFromSource.swiftmodule -parse-as-library %t/Lib.swift -package-name mypkg
// RUN: test -f %t/LibFromSource.swiftmodule

// RUN: %target-swift-frontend -module-name ClientInSamePkgSrc %t/ClientLoadSourceModule.swift -emit-module -emit-module-path %t/ClientInSamePkgSrc.swiftmodule -package-name mypkg -I %t
// RUN: test -f %t/ClientInSamePkgSrc.swiftmodule

//--- Lib.swift
package func log(level: Int) {}

//--- ClientLoadInterfaceModule.swift
import LibFromInterface

func someFun() {
  log(level: 1)
}

//--- ClientLoadSourceModule.swift
import LibFromSource

func someFun() {
  log(level: 1)
}

