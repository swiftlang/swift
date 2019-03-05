// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/opaque_result_type.swift
// RUN: %target-swift-frontend -emit-ir %t/opaque_result_type.swift | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-NODEBUG %t/opaque_result_type.swift

protocol O {
  func bar()
}
protocol O2 {
  func baz()
}

protocol P {
  associatedtype A: O

  func poo() -> A
}
protocol Q: AnyObject {
  associatedtype B: O, O2

  func qoo() -> B
}

extension Int: O, O2 {
  func bar() {}
  func baz() {}
}

extension String: P {
  // CHECK-LABEL: @"$sSS18opaque_result_typeE3pooQryFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
  // -- header: opaque type context (0x4), generic (0x80), unique (0x40)
  // CHECK-SAME:         <i32 0xc4>,
  // -- parent context: module, or anon context for function
  // CHECK-SAME:         @"$s18opaque_result_typeMXM"
  // -- mangled underlying type
  // CHECK-SAME:         @"symbolic Si"
  // -- conformance to O (todo)
  // CHECK-SAME:         i32 0
  // CHECK-SAME:  }>
  func poo() -> __opaque O {
    return 0
  }
}

public class C: P, Q {
  // CHECK-LABEL: @"$s18opaque_result_type1CC3pooQryFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
  // -- header: opaque type context (0x4), generic (0x80), unique (0x40)
  // CHECK-SAME:         <i32 0xc4>
  // -- parent context: module, or anon context for function
  // CHECK-SAME:         @"$s18opaque_result_typeMXM"
  // -- mangled underlying type
  // CHECK-SAME:         @"symbolic Si"
  // -- conformance to O (todo)
  // CHECK-SAME:         i32 0
  // CHECK-SAME:  }>
  func poo() -> __opaque O {
    return 0
  }

  // CHECK-LABEL: @"$s18opaque_result_type1CC3qooQryFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
  // -- header: opaque type context (0x4), generic (0x80), unique (0x40)
  // CHECK-SAME:         <i32 0xc4>
  // -- parent context: module, or anon context for function
  // CHECK-SAME:         @"$s18opaque_result_typeMXM"
  // -- mangled underlying type
  // CHECK-SAME:         @"symbolic Si"
  // -- conformance to O (todo)
  // CHECK-SAME:         i32 0
  // -- conformance to O2 (todo)
  // CHECK-SAME:         i32 0
  // CHECK-SAME:  }>
  func qoo() -> __opaque O & O2 {
    return 0
  }
}

// CHECK-LABEL: @"$s18opaque_result_type3foo1xQrSS_tFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
// -- header: opaque type context (0x4), generic (0x80), unique (0x40)
// CHECK-SAME:         <i32 0xc4>
// -- parent context: module, or anon context for function
// CHECK-SAME:         @"$s18opaque_result_typeMXM"
// -- mangled underlying type
// CHECK-SAME:         @"symbolic SS"
// -- conformance to P (todo)
// CHECK-SAME:         i32 0
// CHECK-SAME:  }>
func foo(x: String) -> __opaque P {
  return x
}

// CHECK-LABEL: @"$s18opaque_result_type3bar1yQrAA1CC_tFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
// -- header: opaque type context (0x4), generic (0x80), unique (0x40)
// CHECK-SAME:         <i32 0xc4>
// -- parent context: module, or anon context for function
// CHECK-SAME:         @"$s18opaque_result_typeMXM"
// -- mangled underlying type
// CHECK-SAME:         @"symbolic _____ 18opaque_result_type1CC"
// -- conformance to Q (todo)
// CHECK-SAME:         i32 0
// CHECK-SAME:  }>
func bar(y: C) -> __opaque Q {
  return y
}

// CHECK-LABEL: @"$s18opaque_result_type3baz1zQrx_tAA1PRzAA1QRzlFQOMQ" = {{.*}} constant <{ {{.*}} }> <{
// -- header: opaque type context (0x4), generic (0x80), unique (0x40), underlying type ordinal 1 (0x1_0000)
// CHECK-SAME:         <i32 0x1_00c4>
// -- parent context: anon context for function
// CHECK-SAME:         @"$s18opaque_result_type3baz1zQrx_tAA1PRzAA1QRzlFMXX"
// -- mangled underlying type
// CHECK-SAME:         @"symbolic x"
// -- conformance to P (todo)
// CHECK-SAME:         i32 0
// -- conformance to Q (todo)
// CHECK-SAME:         i32 0
// CHECK-SAME:  }>
func baz<T: P & Q>(z: T) -> __opaque P & Q {
  return z
}

public func useFoo(x: String, y: C) {
  let p = foo(x: x)
  let pa = p.poo()
  pa.bar()

  let q = bar(y: y)
  let qb = q.qoo()
  qb.bar()
  qb.baz()

  let pq = baz(z: y)
  let pqa = pq.poo()
  pqa.bar()
  let pqb = pq.qoo()
  pqb.bar()
  pqb.baz()
}

// CHECK-LABEL: define {{.*}} @"$s18opaque_result_type3baz1zQrx_tAA1PRzAA1QRzlFQOyAA1CCQo1BQaMa"
// CHECK: call swiftcc %swift.metadata_response @swift_getOpaqueTypeMetadata(i64 0, i8* {{.*}}, %swift.type_descriptor* {{.*}} [[DESCRIPTOR:@"\$s18opaque_result_type3baz1zQrx_tAA1PRzAA1QRzlFQOMQ"]] {{.*}})
// CHECK: call swiftcc i8** @swift_getOpaqueTypeConformance(%swift.type* {{.*}}, %swift.type_descriptor*  {{.*}} [[DESCRIPTOR]] {{.*}}, i64 1)
// CHECK: call swiftcc %swift.metadata_response @swift_getAssociatedTypeWitness

// CHECK-LABEL: define {{.*}} @"$sSS18opaque_result_type1PAA1AAaBP_AA1OPWT"
// CHECK: call swiftcc %swift.metadata_response @swift_getOpaqueTypeMetadata(i64 0, i8* {{.*}}, %swift.type_descriptor* {{.*}} [[DESCRIPTOR:@"\$sSS18opaque_result_typeE3pooQryFQOMQ"]] {{.*}})
// CHECK: call swiftcc i8** @swift_getOpaqueTypeConformance(%swift.type* {{.*}}, %swift.type_descriptor* {{.*}} [[DESCRIPTOR]] {{.*}}, i64 0)
