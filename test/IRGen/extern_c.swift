// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir %t/extern_c.swift -I%t | %FileCheck %s

//--- c_types.h
struct c_struct {
  int field;
};

//--- module.modulemap
module c_types {
  header "c_types.h"
}

//--- extern_c.swift
import c_types

func test() {
  // CHECK: call void @explicit_extern_c()
  explicit_extern_c()
  // CHECK: call void @implicit_extern_c()
  implicit_extern_c()

  // CHECK: call i32 @"+"({{.*}})
  _ = c_struct(field: 1) + c_struct(field: 2)
}

test()

// CHECK: declare void @explicit_extern_c()
@_extern(c, "explicit_extern_c") func explicit_extern_c()

// CHECK: declare void @implicit_extern_c()
@_extern(c) func implicit_extern_c()

// CHECK: declare i32 @"+"({{.*}})
@_extern(c) func +(a: c_struct, b: c_struct) -> c_struct
