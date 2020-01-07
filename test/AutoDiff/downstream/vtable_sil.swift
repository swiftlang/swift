// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// Test JVP/VJP vtable entries for class members:
// - Methods.
// - Accessors (from properties and subscripts).
// - Constructors.

class Super : Differentiable {
  var base: Float
  // FIXME(TF-648): Dummy to make `Super.TangentVector` be nontrivial.
  var _nontrivial: [Float] = []

  // TODO(TF-654): Uncomment attribute when differentiation supports class initializers.
  // TODO(TF-645): Remove `vjpInit` when differentiation supports `ref_element_addr`.
  // @differentiable(vjp: vjpInit)
  init(base: Float) {
    self.base = base
  }
  static func vjpInit(base: Float) -> (Super, (TangentVector) -> Float) {
    return (Super(base: base), { x in x.base })
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
  // TODO(TF-654): Uncomment attribute when differentiation supports class initializers.
  // TODO(TF-645): Remove `vjpInit2` when differentiation supports `ref_element_addr`.
  // @differentiable(vjp: vjpInit2)
  override init(base: Float) {
    super.init(base: base)
    self.base = base
  }
  static func vjpInit2(base: Float) -> (Sub, (TangentVector) -> Float) {
    return (Sub(base: base), { x in x.base })
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
// CHECK-NEXT:   #Super.base!getter.1: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg
// CHECK-NEXT:   #Super.base!setter.1: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs
// CHECK-NEXT:   #Super.base!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM
// CHECK-NEXT:   #Super._nontrivial!getter.1: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg
// CHECK-NEXT:   #Super._nontrivial!setter.1: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs
// CHECK-NEXT:   #Super._nontrivial!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM
// CHECK-NEXT:   #Super.init!allocator.1: (Super.Type) -> (Float) -> Super : @$s10vtable_sil5SuperC4baseACSf_tcfC
// CHECK-NEXT:   #Super.property!getter.1: (Super) -> () -> Float : @$s10vtable_sil5SuperC8propertySfvg
// CHECK-NEXT:   #Super.property!getter.1.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil5SuperC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.property!getter.1.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil5SuperC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.generic!1: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF
// CHECK-NEXT:   #Super.generic!1.jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk
// CHECK-NEXT:   #Super.generic!1.vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk
// CHECK-NEXT:   #Super.f!1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil5SuperC1fyS2f_SftF
// CHECK-NEXT:   #Super.f!1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.f!1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.subscript!getter.1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil5SuperCyS2f_Sftcig
// CHECK-NEXT:   #Super.subscript!getter.1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.subscript!getter.1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil5SuperCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK-NEXT:   #Super.move!1: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF
// CHECK-NEXT:   #Super.deinit!deallocator.1: @$s10vtable_sil5SuperCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:   #Super.base!getter.1: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg [inherited]
// CHECK-NEXT:   #Super.base!setter.1: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs [inherited]
// CHECK-NEXT:   #Super.base!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM [inherited]
// CHECK-NEXT:   #Super._nontrivial!getter.1: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg [inherited]
// CHECK-NEXT:   #Super._nontrivial!setter.1: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs [inherited]
// CHECK-NEXT:   #Super._nontrivial!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM [inherited]
// CHECK-NEXT:   #Super.init!allocator.1: (Super.Type) -> (Float) -> Super : @$s10vtable_sil3SubC4baseACSf_tcfC [override]
// CHECK-NEXT:   #Super.property!getter.1: (Super) -> () -> Float : @$s10vtable_sil3SubC8propertySfvg [override]
// CHECK-NEXT:   #Super.property!getter.1.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.property!getter.1.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.generic!1: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF [inherited]
// CHECK-NEXT:   #Super.generic!1.jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic!1.vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f!1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubC1fyS2f_SftF [override]
// CHECK-NEXT:   #Super.f!1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.f!1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.subscript!getter.1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubCyS2f_Sftcig [override]
// CHECK-NEXT:   #Super.subscript!getter.1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.subscript!getter.1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK-NEXT:   #Super.move!1: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF [inherited]
// CHECK-NEXT:   #Sub.f!1.jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK-NEXT:   #Sub.f!1.vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK-NEXT:   #Sub.deinit!deallocator.1: @$s10vtable_sil3SubCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable SubSub {
// CHECK-NEXT:   #Super.base!getter.1: (Super) -> () -> Float : @$s10vtable_sil5SuperC4baseSfvg [inherited]
// CHECK-NEXT:   #Super.base!setter.1: (Super) -> (Float) -> () : @$s10vtable_sil5SuperC4baseSfvs [inherited]
// CHECK-NEXT:   #Super.base!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC4baseSfvM [inherited]
// CHECK-NEXT:   #Super._nontrivial!getter.1: (Super) -> () -> [Float] : @$s10vtable_sil5SuperC11_nontrivialSaySfGvg [inherited]
// CHECK-NEXT:   #Super._nontrivial!setter.1: (Super) -> ([Float]) -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvs [inherited]
// CHECK-NEXT:   #Super._nontrivial!modify.1: (Super) -> () -> () : @$s10vtable_sil5SuperC11_nontrivialSaySfGvM [inherited]
// CHECK-NEXT:   #Super.init!allocator.1: (Super.Type) -> (Float) -> Super : @$s10vtable_sil03SubC0C4baseACSf_tcfC [override]
// CHECK-NEXT:   #Super.property!getter.1: (Super) -> () -> Float : @$s10vtable_sil3SubC8propertySfvg [inherited]
// CHECK-NEXT:   #Super.property!getter.1.jvp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.property!getter.1.vjp.S: (Super) -> () -> Float : @AD__$s10vtable_sil3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic!1: <T> (Super) -> (T, T) -> T : @$s10vtable_sil5SuperC7genericyxx_xtlF [inherited]
// CHECK-NEXT:   #Super.generic!1.jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__jvp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.generic!1.vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s10vtable_sil5SuperC7genericyxx_xtlF__vjp_src_0_wrt_0_s14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f!1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubC1fyS2f_SftF [inherited]
// CHECK-NEXT:   #Super.f!1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.f!1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.subscript!getter.1: (Super) -> (Float, Float) -> Float : @$s10vtable_sil3SubCyS2f_Sftcig [inherited]
// CHECK-NEXT:   #Super.subscript!getter.1.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.subscript!getter.1.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Super.move!1: (Super) -> (Super.TangentVector) -> () : @$s10vtable_sil5SuperC4move5alongyAC13TangentVectorV_tF [inherited]
// CHECK-NEXT:   #Sub.f!1.jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #Sub.f!1.vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s10vtable_sil3SubC1fyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK-NEXT:   #SubSub.deinit!deallocator.1: @$s10vtable_sil03SubC0CfD
// CHECK-NEXT: }
