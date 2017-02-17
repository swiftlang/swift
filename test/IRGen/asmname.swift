// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386 || CPU=x86_64

@_silgen_name("atan2") func atan2test(_ a: Double, _ b: Double) -> Double

atan2test(0.0, 0.0)

// CHECK: call swiftcc double @atan2(double {{.*}}, double {{.*}})
