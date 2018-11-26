
// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s | %FileCheck %s

// SR-3090:

class Box<T> {
    public let value: T
    
    public init(_ value: T) {
        self.value = value
    }
}

// CHECK: sil @$S4main7testBoxyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   // function_ref closure #1 in testBox()
// CHECK:   [[CLOSURE:%.*]] = function_ref @$S4main7testBoxyyFyycfU_ : $@convention(thin) () -> ()
// CHECK:   [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) () -> () to $@callee_guaranteed () -> ()
// CHECK:   [[TUPLEA:%.*]] = tuple (%{{.*}} : $Int, [[THICK]] : $@callee_guaranteed () -> ())
// CHECK:   ([[ELTA_0:%.*]], [[ELTA_1:%.*]]) = destructure_tuple [[TUPLEA]] : $(Int, @callee_guaranteed () -> ())
// CHECK:   [[THUNK1:%.*]] = function_ref @$SIeg_ytytIegnr_TR : $@convention(thin) (@in_guaranteed (), @guaranteed @callee_guaranteed () -> ()) -> @out ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK1]]([[ELTA_1]]) : $@convention(thin) (@in_guaranteed (), @guaranteed @callee_guaranteed () -> ()) -> @out ()
// CHECK:   [[TUPLEB:%.*]] = tuple ([[ELTA_0]] : $Int, [[PA]] : $@callee_guaranteed (@in_guaranteed ()) -> @out ())
// CHECK:   ([[TUPLEB_0:%.*]], [[TUPLEB_1:%.*]]) = destructure_tuple [[TUPLEB]]
// CHECK:   // function_ref Box.__allocating_init(_:)
// CHECK:   [[INIT_F:%.*]] = function_ref @$S4main3BoxCyACyxGxcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thick Box<τ_0_0>.Type) -> @owned Box<τ_0_0>
// CHECK:   [[CALL:%.*]] = apply [[INIT_F]]<(Int, () -> ())>(%{{.*}}, %{{.*}}) : $@convention(method) <τ_0_0> (@in τ_0_0, @thick Box<τ_0_0>.Type) -> @owned Box<τ_0_0>
// CHECK:   [[BORROW_CALL:%.*]] = begin_borrow [[CALL]] : $Box<(Int, () -> ())> 
// CHECK:   [[REF:%.*]] = ref_element_addr [[BORROW_CALL]] : $Box<(Int, () -> ())>, #Box.value
// CHECK:   [[TUPLEC:%.*]] = load [copy] [[REF]] : $*(Int, @callee_guaranteed (@in_guaranteed ()) -> @out ())
// CHECK:   ([[TUPLEC_0:%.*]], [[TUPLEC_1:%.*]]) = destructure_tuple [[TUPLEC]]
// CHECK:   [[THUNK2:%.*]] = function_ref @$SytytIegnr_Ieg_TR : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed ()) -> @out ()) -> ()
// CHECK:   [[PA2:%.*]] = partial_apply [callee_guaranteed] [[THUNK2]]([[TUPLEC_1]]) : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed ()) -> @out ()) -> ()
// CHECK:   destroy_value [[PA2]] : $@callee_guaranteed () -> ()    
// CHECK:   end_borrow [[BORROW_CALL]] from %{{.*}} : $Box<(Int, () -> ())>, $Box<(Int, () -> ())>
// CHECK-LABEL: } // end sil function '$S4main7testBoxyyF'
public func testBox() {
  let box = Box((22, { () in }))
  let foo = box.value.0
  print(foo)
}


// Another crash -- re-abstracting function type inside optional in tuple
// in-place

func g<T>() -> (Int, T)? { }

func f<T>(t: T) {
  let _: (Int, ((T) -> (), T))? = g()
}
