// RUN: %target-swift-emit-ir -Onone %s -parse-as-library -module-name main \
// RUN:   -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking | %FileCheck %s

// Copying and destroying an enum whose payloads are all foreign reference types
// must call each payload's own retain/release functions.

import MultiPayloadEnum

enum Either { case a(A), b(B) }
enum EitherOrNone { case a(A), b(B), none }

func copy(_ e: Either) -> (Either, Either) { (e, e) }
func drop(_ e: consuming Either) {}
func copy(_ e: EitherOrNone) -> (EitherOrNone, EitherOrNone) { (e, e) }
func drop(_ e: consuming EitherOrNone) {}

// CHECK-DAG: call void @{{.*}}retainA{{.*}}(ptr
// CHECK-DAG: call void @{{.*}}retainB{{.*}}(ptr
// CHECK-DAG: call void @{{.*}}releaseA{{.*}}(ptr
// CHECK-DAG: call void @{{.*}}releaseB{{.*}}(ptr
