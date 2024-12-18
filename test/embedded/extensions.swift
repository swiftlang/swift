// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-ir     -I %t                      %t/Main.swift     -parse-stdlib -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public protocol MyProtocol {}

// BEGIN Main.swift

import MyModule

struct MyStruct {}

extension MyStruct: MyProtocol {}

// CHECK: define {{.*}}i32 @main(i32 %0, ptr %1)
// CHECK-NOT: MyStruct
// CHECK-NOT: MyProtocol
