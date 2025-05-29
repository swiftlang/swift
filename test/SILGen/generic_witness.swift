
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name generic_witness %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name generic_witness %s

protocol Runcible {
  func runce<A>(_ x: A)
}

// CHECK-LABEL: sil hidden [ossa] @$s15generic_witness3foo{{[_0-9a-zA-Z]*}}F : $@convention(thin) <B where B : Runcible> (@in_guaranteed B) -> () {

func foo<B : Runcible>(_ x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce : {{.*}} : $@convention(witness_method: Runcible) <τ_0_0 where τ_0_0 : Runcible><τ_1_0> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil hidden [ossa] @$s15generic_witness3bar{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@in_guaranteed any Runcible) -> ()
func bar(_ x: Runcible) {
  var x = x
  // CHECK: [[BOX:%.*]] = alloc_box ${ var any Runcible }
  // CHECK: [[TEMP:%.*]] = alloc_stack $any Runcible
  // CHECK: [[EXIST:%.*]] = open_existential_addr immutable_access [[TEMP]] : $*any Runcible to $*[[OPENED:@opened\(.*, any Runcible\) Self]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #Runcible.runce :
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

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s15generic_witness6CanvasVyxGAA6MediumA2aEP4draw5paint6pencily6StrokeQyd___qd__tAA6PencilRd__7Texture_5PaintQZAKRSlFTW : $@convention(witness_method: Medium) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in_guaranteed τ_0_0.Paint, @in_guaranteed τ_1_0, @in_guaranteed Canvas<τ_0_0>) -> () {
// CHECK: [[FN:%.*]] = function_ref @$s15generic_witness6CanvasV4draw5paint6pencily5PaintQz_qd__tAA6PencilRd__6StrokeQyd__AHRSlF : $@convention(method) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in_guaranteed τ_0_0.Paint, @in_guaranteed τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: apply [[FN]]<τ_0_0, τ_1_0>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : Ink><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in_guaranteed τ_0_0.Paint, @in_guaranteed τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: }
