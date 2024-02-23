// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg | %FileCheck %s --check-prefixes=CHECK,CHECK-DEFAULT
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK,CHECK-DEFAULT

// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience | %FileCheck %s --check-prefixes=CHECK,CHECK-BYPASS
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK,CHECK-BYPASS

//--- Utils.swift
package struct PkgStruct {
  package var pkgVar = 1
  package init() {}
}

public struct PubStruct {
  public var pubVar = 1
  public init() {}
}

//--- Client.swift
import Utils

func foo() {
  print(PkgStruct().pkgVar)
}

// CHECK: sil hidden [ossa] @$s6Client3fooyyF : $@convention(thin) () -> () {
// CHECK-DEFAULT: [[F_REF:%.*]] = function_ref @$s5Utils9PkgStructV6pkgVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-DEFAULT: sil package_external @$s5Utils9PkgStructV6pkgVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-BYPASS:  [[ADDR:%.*]] = struct_element_addr {{.*}} : $*PkgStruct, #PkgStruct.pkgVar

func bar() {
  print(PubStruct().pubVar)
}

// CHECK: sil hidden [ossa] @$s6Client3baryyF : $@convention(thin) () -> () {
// CHECK-DEFAULT: [[F_REF:%.*]] = function_ref @$s5Utils9PubStructV6pubVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-DEFAULT: sil @$s5Utils9PubStructV6pubVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-BYPASS:  [[ADDR:%.*]] = struct_element_addr {{.*}} : $*PubStruct, #PubStruct.pubVar
