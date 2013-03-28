// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[B:%C5casts1B]] = type
// CHECK: [[D:%C5casts1D]] = type

class B { }
class D : B { }

// CHECK: define [[B]]* @_T5casts6upcastFT1dCS_1D_CS_1B
func upcast(d:D) -> B {
  // CHECK: bitcast [[D]]* {{%.*}} to [[B]]*
  return d as B
}
// CHECK: }

// CHECK: define [[D]]* @_T5casts8downcastFT1bCS_1B_CS_1D
func downcast(b:B) -> D {
  // CHECK: bitcast [[B]]* {{%.*}} to i8*
  // CHECK-NEXT: call i8* @swift_dynamicCastClassUnconditional(i8* {{%.*}}, i8* bitcast ({{.*}} @_TMdC5casts1D {{.*}}))
  return b as! D
}
// CHECK: }

// CHECK: define i1 @_T5casts3isaFT1bCS_1B_Sb
func isa(b:B) -> Bool {
  // CHECK: bitcast [[B]]* {{%.*}} to i8*
  // CHECK-NEXT: call i8* @swift_dynamicCastClass(i8* {{%.*}}, i8* bitcast ({{.*}} @_TMdC5casts1D {{.*}}))
  // CHECK-NEXT: icmp ne i8* {{.*}}, null
  return b is D
}
// CHECK: }
