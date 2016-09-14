// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Runcible {
  func runce<A>(_ x: A)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3foo{{.*}} : $@convention(thin) <B where B : Runcible> (@in B) -> () {

func foo<B : Runcible>(_ x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : Runcible><τ_1_0> (@in τ_1_0, @in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3bar{{.*}} : $@convention(thin) (@in Runcible) -> ()
func bar(_ x: Runcible) {
  var x = x
  // CHECK: [[BOX:%.*]] = alloc_box $Runcible
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

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWuRx15generic_witness3Inkwx5PaintS_3PenrGVS_6Canvasx_S_6MediumS_FS4_4drawuRd__S_6Pencilwd__6StrokezWx7TextureS1__rfT5paintWxS7_S1__6pencilqd___T_ : $@convention(witness_method) <I where I : Ink, I.Paint : Pen><P where P : Pencil> (@in I.Paint, @in P, @in_guaranteed Canvas<I>) -> () {
// CHECK: [[FN:%.*]] = function_ref @_TFV15generic_witness6Canvas4drawuRd__S_6Pencilwx5Paintzwd__6StrokerfT5paintwxS2_6pencilqd___T_ : $@convention(method) <τ_0_0 where τ_0_0 : Ink, τ_0_0.Paint : Pen><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in τ_0_0.Paint, @in τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: apply [[FN]]<I, P, I.Paint>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : Ink, τ_0_0.Paint : Pen><τ_1_0 where τ_1_0 : Pencil, τ_0_0.Paint == τ_1_0.Stroke> (@in τ_0_0.Paint, @in τ_1_0, Canvas<τ_0_0>) -> ()
// CHECK: }
