// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// Test JVP/VJP vtable entries for class members:
// - Methods.
// - Accessors (from properties and subscripts).
// - Constructors.

class Super : Differentiable {
  var base: Float
  // FIXME(TF-648): Dummy to make `Super.TangentVector` be nontrivial.
  var _nontrivial: [Float] = []

  init(base: Float) {
    self.base = base
  }

  @differentiable
  var property: Float { base }
  @derivative(of: property)
  final func vjpProperty() -> (value: Float, pullback: (Float) -> TangentVector) {
    return (property, { _ in .zero })
  }

  @differentiable(wrt: x where T: Differentiable)
  func generic<T>(_ x: T, _ y: T) -> T {
    return x
  }

  @differentiable(wrt: x)
  func f(_ x: Float, _ y: Float) -> Float {
    return x * y
  }

  @differentiable(wrt: x)
  subscript(_ x: Float, _ y: Float) -> Float {
    return x * y
  }

  @derivative(of: f, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func jvpf(_ x: Float, _ y: Float) -> (value: Float, differential: (Float) -> Float) {
    return (f(x, y), { v in v * y })
  }
  @derivative(of: f, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func vjpf(_ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
    return (f(x, y), { v in v * y })
  }
}

class Sub : Super {
  override init(base: Float) {
    super.init(base: base)
  }

  @differentiable
  override var property: Float { base }
  @derivative(of: property)
  final func vjpProperty2() -> (value: Float, pullback: (Float) -> TangentVector) {
    return (property, { _ in .zero })
  }

  @differentiable(wrt: x)
  // New `@differentiable` attribute.
  @differentiable(wrt: (x, y))
  override func f(_ x: Float, _ y: Float) -> Float {
    return x * y
  }

  @differentiable(wrt: x)
  override subscript(_ x: Float, _ y: Float) -> Float {
    return x * y
  }

  @derivative(of: f, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func jvpf2(_ x: Float, _ y: Float) -> (value: Float, differential: (Float) -> Float) {
    return (f(x, y), { v in v * y })
  }
  @derivative(of: f, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func vjpf2(_ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
    return (f(x, y), { v in v * y })
  }
}

class SubSub : Sub {}

// CHECK-LABEL: sil_vtable Super {
// CHECK-NEXT:   #Super.base!getter: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg
// CHECK-NEXT:   #Super.base!setter: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs
// CHECK-NEXT:   #Super.base!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM
// CHECK-NEXT:   #Super._nontrivial!getter: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg
// CHECK-NEXT:   #Super._nontrivial!setter: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs
// CHECK-NEXT:   #Super._nontrivial!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM
// CHECK-NEXT:   #Super.init!allocator: (Super.Type) -> (Float) -> Super : @$s10vtable_sil5SuperC4baseACSf_tcfC
// CHECK-NEXT:   #Super.property!getter: (Super) -> () -> Float : @$s10vtable_sil5SuperC8propertySfvg
// CHECK-NEXT:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil5SuperC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil5SuperC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.generic: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF
// CHECK-NEXT:   #Super.generic!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk
// CHECK-NEXT:   #Super.generic!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk
// CHECK-NEXT:   #Super.f: (Super) -> (Float, Float) -> Float : @$s10vtable_sil5SuperC1fyS2f_SftF
// CHECK-NEXT:   #Super.f!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.f!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s10vtable_sil5SuperCyS2f_Sftcig
// CHECK-NEXT:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.move: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF
// CHECK-NEXT:   #Super.deinit!deallocator: @$s10vtable_sil5SuperCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:   #Super.base!getter: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg [inherited]
// CHECK-NEXT:   #Super.base!setter: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs [inherited]
// CHECK-NEXT:   #Super.base!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM [inherited]
// CHECK-NEXT:   #Super._nontrivial!getter: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg [inherited]
// CHECK-NEXT:   #Super._nontrivial!setter: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs [inherited]
// CHECK-NEXT:   #Super._nontrivial!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM [inherited]
// CHECK-NEXT:   #Super.init!allocator: (Super.Type) -> (Float) -> Super : @$s10vtable_sil3SubC4baseACSf_tcfC [override]
// CHECK-NEXT:   #Super.property!getter: (Super) -> () -> Float : @$s10vtable_sil3SubC8propertySfvg [override]
// CHECK-NEXT:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.generic: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF [inherited]
// CHECK-NEXT:   #Super.generic!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubC1fyS2f_SftF [override]
// CHECK-NEXT:   #Super.f!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.f!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubCyS2f_Sftcig [override]
// CHECK-NEXT:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.move: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF [inherited]
// CHECK-NEXT:   #Sub.f!jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK-NEXT:   #Sub.f!vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK-NEXT:   #Sub.deinit!deallocator: @$s10vtable_sil3SubCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable SubSub {
// CHECK-NEXT:   #Super.base!getter: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg [inherited]
// CHECK-NEXT:   #Super.base!setter: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs [inherited]
// CHECK-NEXT:   #Super.base!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM [inherited]
// CHECK-NEXT:   #Super._nontrivial!getter: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg [inherited]
// CHECK-NEXT:   #Super._nontrivial!setter: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs [inherited]
// CHECK-NEXT:   #Super._nontrivial!modify: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM [inherited]
// CHECK-NEXT:   #Super.init!allocator: (Super.Type) -> (Float) -> Super : @$s10vtable_sil03SubC0C4baseACSf_tcfC [override]
// CHECK-NEXT:   #Super.property!getter: (Super) -> () -> Float : @$s10vtable_sil3SubC8propertySfvg [inherited]
// CHECK-NEXT:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF [inherited]
// CHECK-NEXT:   #Super.generic!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubC1fyS2f_SftF [inherited]
// CHECK-NEXT:   #Super.f!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubCyS2f_Sftcig [inherited]
// CHECK-NEXT:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.move: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF [inherited]
// CHECK-NEXT:   #Sub.f!jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Sub.f!vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #SubSub.deinit!deallocator: @$s10vtable_sil03SubC0CfD
// CHECK-NEXT: }
