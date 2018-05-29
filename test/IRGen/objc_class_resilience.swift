// RUN: %empty-directory(%t)
// RUN: %utils/chex.py < %s > %t/objc_class_resilience.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t)  -I %t -emit-ir -enable-resilience %t/objc_class_resilience.swift | %FileCheck %t/objc_class_resilience.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t)  -I %t -emit-ir -enable-resilience -O %t/objc_class_resilience.swift

// REQUIRES: objc_interop

import ObjectiveC
import resilient_struct

// CHECK-32-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC5firstSivpWvd" = hidden global i32 4
// CHECK-64-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC5firstSivpWvd" = hidden global i64 8

// CHECK-32-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC6second16resilient_struct4SizeVvpWvd" = hidden global i32 8
// CHECK-64-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC6second16resilient_struct4SizeVvpWvd" = hidden global i64 16

// CHECK-32-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC5thirdSivpWvd" = hidden global i32 16
// CHECK-64-LABEL: @"$S21objc_class_resilience23ClassWithResilientFieldC5thirdSivpWvd" = hidden global i64 32

// CHECK-LABEL: @"OBJC_CLASS_$__TtC21objc_class_resilience23ClassWithResilientField" = alias

// CHECK-LABEL: define hidden swiftcc {{i32|i64}} @"$S21objc_class_resilience23ClassWithResilientFieldC5firstSivg"(%T21objc_class_resilience23ClassWithResilientFieldC* swiftself) {{.*}} {
// CHECK: %offset = load [[INT]], [[INT]]* @"$S21objc_class_resilience23ClassWithResilientFieldC5firstSivpWvd"
// CHECK: }

// CHECK-LABEL: define hidden swiftcc void @"$S21objc_class_resilience23ClassWithResilientFieldC6second16resilient_struct4SizeVvg"(%swift.opaque* noalias nocapture sret, %T21objc_class_resilience23ClassWithResilientFieldC* swiftself) {{.*}} {
// CHECK: %offset = load [[INT]], [[INT]]* @"$S21objc_class_resilience23ClassWithResilientFieldC6second16resilient_struct4SizeVvpWvd"
// CHECK: }

// CHECK-LABEL: define hidden swiftcc {{i32|i64}} @"$S21objc_class_resilience23ClassWithResilientFieldC5thirdSivg"(%T21objc_class_resilience23ClassWithResilientFieldC* swiftself) {{.*}} {
// CHECK: %offset = load [[INT]], [[INT]]* @"$S21objc_class_resilience23ClassWithResilientFieldC5thirdSivpWvd"
// CHECK: }

public class ClassWithResilientField : NSObject {
  var first: Int
  var second: Size
  var third: Int

  init(x: Int, y: Size, z: Int) {
    self.first = x
    self.second = y
    self.third = z
  }
}
