// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// 1. Non-resilient access at the use site to resilient decls defined in another module is allowed if
/// in the same package and optimization is enabled with -experimental-package-bypass-resilience.
// RUN: %target-swift-frontend %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg | %FileCheck %s --check-prefixes=CHECK,CHECK-DEFAULT
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK,CHECK-DEFAULT

// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience | %FileCheck %s --check-prefixes=CHECK,CHECK-BYPASS
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK,CHECK-BYPASS

/// 2. Non-resilient access to decls from another module built with -enable-testing should be
/// disallowed even if optimization is enabled.
// RUN: rm -rf %t/Utils.swiftmodule
// RUN: %target-swift-frontend %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-testing \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience | %FileCheck %s --check-prefixes=CHECK,CHECK-TESTING

/// 3. Non-resilient access to decls from another module built with -experimental-skip-non-exportable-decls
/// should be disallowed even if optimization is enabled.
// RUN: rm -rf %t/Utils.swiftmodule
// RUN: %target-swift-frontend %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -experimental-skip-non-exportable-decls \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: llvm-bcanalyzer --dump %t/Utils.swiftmodule | %FileCheck %s --check-prefix=CHECK-BC
// CHECK-BC: ONLY_HAS_EXPORTABLE_DECLS

// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-bypass-resilience | %FileCheck %s --check-prefixes=CHECK,CHECK-BODY

/// 4. Non-resilient access to decls from another module built from .package.swiftinterface
/// should be disallowed even if optimization is enabled.
// RUN: rm -rf %t/Utils.swiftmodule
// RUN: %target-swift-frontend %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module \
// RUN:   -emit-module-interface-path %t/Utils.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Utils.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Utils.package.swiftinterface
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name Client -package-name mypkg -experimental-package-interface-load -experimental-package-bypass-resilience | %FileCheck %s --check-prefixes=CHECK,CHECK-BODY

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
// CHECK-BODY-NOT: struct_element_addr {{.*}} : $*PkgStruct, #PkgStruct.pkgVar
// CHECK-BODY: function_ref @$s5Utils9PkgStructV6pkgVarSivg
// CHECK-DEFAULT: function_ref @$s5Utils9PkgStructV6pkgVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-DEFAULT: sil package_external @$s5Utils9PkgStructV6pkgVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-BYPASS:  struct_element_addr {{.*}} : $*PkgStruct, #PkgStruct.pkgVar
// CHECK-TESTING: struct_extract {{.*}} : $PkgStruct, #PkgStruct.pkgVar

func bar() {
  print(PubStruct().pubVar)
}

// CHECK: sil hidden [ossa] @$s6Client3baryyF : $@convention(thin) () -> () {
// CHECK-BODY-NOT: struct_element_addr {{.*}} : $*PubStruct, #PubStruct.pubVar
// CHECK-BODY: function_ref @$s5Utils9PubStructV6pubVarSivg
// CHECK-DEFAULT: function_ref @$s5Utils9PubStructV6pubVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-DEFAULT: sil @$s5Utils9PubStructV6pubVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-BYPASS:  struct_element_addr {{.*}} : $*PubStruct, #PubStruct.pubVar
// CHECK-TESTING: struct_extract {{.*}} : $PubStruct, #PubStruct.pubVar
