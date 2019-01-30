// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

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
  func poo() -> Int /*TODO: __opaque O*/ {
    return 0
  }
}

public class C: P, Q {
  func poo() -> Int /*TODO: __opaque O*/ {
    return 0
  }
  func qoo() -> Int /*TODO: __opaque O & O2*/ {
    return 0
  }
}

func foo(x: String) -> __opaque P {
  return x
}

func bar(y: C) -> __opaque Q {
  return y
}

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
// CHECK: call swiftcc %swift.metadata_response @swift_getOpaqueTypeMetadata(i64 0, i8* {{.*}}, %swift.type_descriptor* [[DESCRIPTOR:@"\$s18opaque_result_type3baz1zQrx_tAA1PRzAA1QRzlFQOMQ"]])
// CHECK: call swiftcc i8** @swift_getOpaqueTypeConformance(%swift.type* {{.*}}, %swift.type_descriptor* [[DESCRIPTOR]], i64 1)
// CHECK: call swiftcc %swift.metadata_response @swift_getAssociatedTypeWitness
