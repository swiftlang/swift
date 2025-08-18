// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s
// REQUIRES: CPU=x86_64 || CPU=arm64

func f(_ x: Int, _ y: UInt32) -> Bool { x < y }
// CHECK-LABEL: define {{.*}} @"$s4main1fySbSi_s6UInt32VtF"(i64 %0, i32 %1)
// CHECK: %2 = zext i32 %1 to i64
// CHECK-NEXT: %3 = icmp sgt i64 %2, %0
// CHECK-NEXT: ret i1 %3

func g(_ x: UInt32, _ y: Int) -> Bool { x < y }
// CHECK-LABEL: define {{.*}} @"$s4main1gySbs6UInt32V_SitF"(i32 %0, i64 %1)
// CHECK: %2 = zext i32 %0 to i64
// CHECK-NEXT: %3 = icmp slt i64 %2, %1
// CHECK-NEXT: ret i1 %3
