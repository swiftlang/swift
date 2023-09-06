// RUN: %target-swift-frontend -emit-sil -enable-lexical-lifetimes=false -O -module-name=main %s | %FileCheck %s

// REQUIRES: swift_in_compiler

class C {}

@_silgen_name("borrow")
func borrow(_ c: __shared C)

@_silgen_name("barrier")
func barrier()

// CHECK-LABEL: sil {{.*}} [lexical_lifetimes] @funky : {{.*}} {
// CHECK:         [[INSTANCE:%[^,]+]] = alloc_ref $C                               
// CHECK:         [[EI:%.*]] = end_init_let_ref [[INSTANCE]]
// CHECK:         [[BORROW:%[^,]+]] = function_ref @borrow
// CHECK:         apply [[BORROW]]([[EI]])
// CHECK:         [[BARRIER:%[^,]+]] = function_ref @barrier
// CHECK:         apply [[BARRIER]]()
// CHECK:         strong_release [[EI]]
// CHECK-LABEL: } // end sil function 'funky'
@_silgen_name("funky")
@_lexicalLifetimes
func funky() {
  let c = C()
  borrow(c)
  barrier()
}
