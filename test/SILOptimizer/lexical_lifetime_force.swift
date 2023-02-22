// RUN: %target-swift-frontend -emit-sil -enable-lexical-lifetimes=false -O -module-name=main %s | %FileCheck %s

class C {}

@_silgen_name("borrow")
func borrow(_ c: __shared C)

@_silgen_name("barrier")
func barrier()

// CHECK-LABEL: sil {{.*}} [lexical_lifetimes] @funky : {{.*}} {
// CHECK:         [[INSTANCE:%[^,]+]] = alloc_ref $C                               
// CHECK:         [[BORROW:%[^,]+]] = function_ref @borrow
// CHECK:         apply [[BORROW]]([[INSTANCE]])
// CHECK:         [[BARRIER:%[^,]+]] = function_ref @barrier
// CHECK:         apply [[BARRIER]]()
// CHECK:         strong_release [[INSTANCE]]
// CHECK-LABEL: } // end sil function 'funky'
@_silgen_name("funky")
@_lexicalLifetimes
func funky() {
  let c = C()
  borrow(c)
  barrier()
}
