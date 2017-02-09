// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

protocol Runcible {
  func runce<A>(_ x: A)
}

// CHECK-LABEL: sil hidden @_T015generic_witness3foo{{[_0-9a-zA-Z]*}}F : $@convention(thin) <B where B : Runcible> (@in B) -> () {

func foo<B : Runcible>(_ x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce!1 : {{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : Runcible><τ_1_0> (@in τ_1_0, @in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil hidden @_T015generic_witness3bar{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@in Runcible) -> ()
func bar(_ x: Runcible) {
  var x = x
  // CHECK: [[BOX:%.*]] = alloc_box ${ var Runcible }
  // CHECK: [[TEMP:%.*]] = alloc_stack $Runcible
  // CHECK: [[EXIST:%.*]] = open_existential_addr [[TEMP]] : $*Runcible to $*[[OPENED:@opened(.*) Runcible]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #Runcible.runce!1
  // CHECK: apply [[METHOD]]<[[OPENED]], Int>
  x.runce(5)
}

protocol Color {}

protocol Ink {
    associatedtype Paint
}

protocol Pen {}

protocol Pencil : Pen {
    associatedtype Stroke : Pen
}

protocol Medium {
    associatedtype Texture : Ink

    func draw<P : Pencil>(paint: Texture.Paint, pencil: P) where P.Stroke == Texture.Paint
}

struct Canvas<I : Ink> where I.Paint : Pen {
    typealias Texture = I

    func draw<P : Pencil>(paint: I.Paint, pencil: P) where P.Stroke == Texture.Paint { }
}

extension Canvas : Medium {}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T015generic_witness6CanvasVyxGAA6MediumAaA3InkRzAA3Pen5PaintRpzlAaDP4drawy7Texture_AGQZ5paint_qd__6penciltAA6PencilRd__AL6StrokeRtd__lFTW : $@convention(witness_method) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in τ_0_0.Paint, @in τ_1_0, @in_guaranteed Canvas<τ_0_0>) -> () {
// CHECK: [[FN:%.*]] = function_ref @_T015generic_witness6CanvasV4drawy5PaintQz5paint_qd__6penciltAA6PencilRd__6StrokeQyd__AFRSlF : $@convention(method) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in τ_0_0.Paint, @in τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: apply [[FN]]<τ_0_0, τ_1_0>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in τ_0_0.Paint, @in τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: }
