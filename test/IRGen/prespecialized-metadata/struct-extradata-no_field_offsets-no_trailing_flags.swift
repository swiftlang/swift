// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

//     CHECK: @"$s4main4PairVMP" = internal constant <{
// Ensure that there is no reference to an anonymous global from within
// s4main4PairVMP.  It would be better to use CHECK-NOT-SAME, if such a thing
// existed.
// CHECK-NOT: {{@[0-9]+}}
// CHECK-LABEL: @"symbolic _____ 4main4PairV"

struct Pair<First, Second, Third> {
    let first: First
    let second: Second
    let third: Third
}



