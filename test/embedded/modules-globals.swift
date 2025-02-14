// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public var global_in_module_used_in_module = 0
public var global_in_module_unused_in_module = 0

public func foo() {
  global_in_module_used_in_module += 1
}

// BEGIN Main.swift

import MyModule

public var global_in_client_used_in_client = 0
public var global_in_client_unused_in_client = 0

public func main() {
  global_in_module_used_in_module = 42
  global_in_module_unused_in_module = 42
  global_in_client_used_in_client = 42
  foo()
}

// CHECK: @"$e4Main022global_in_client_used_c1_D0Sivp" = {{.*}}global %TSi zeroinitializer
// CHECK: @"$e4Main024global_in_client_unused_c1_D0Sivp" = {{.*}}global %TSi zeroinitializer
// CHECK: @"$e8MyModule022global_in_module_used_d1_E0Sivp" = {{.*}}global %TSi zeroinitializer
// CHECK: @"$e8MyModule024global_in_module_unused_d1_E0Sivp" = {{.*}}global %TSi zeroinitializer
