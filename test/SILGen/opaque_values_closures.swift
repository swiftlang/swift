// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -disable-availability-checking -enable-sil-opaque-values -Xllvm -sil-full-demangle -primary-file %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// Test SILGen -enable-sil-opaque-values with tests that depend on the stdlib.

// CaptureKind
// - Box
//   - opaque?
//   - canGuarantee?
//   - captureCanEscape?
//   - !isPack! -- assert(!isPack())
//   opaque=true  canGuarantee=true  captureCanEscape=true  [x] // captureBoxOpaqueGuaranteedEscaping
//   opaque=true  canGuarantee=false captureCanEscape=true  [x] // captureBoxOpaqueOwnedEscaping
//   opaque=true  canGuarantee=false captureCanEscape=false [-] // captureBoxOpaqueOwnedNonescaping // rdar://114195999
//   opaque=false canGuarantee=true  captureCanEscape=true  [x] // captureBoxNonopaqueGuaranteedEscaping
//   opaque=false canGuarantee=false captureCanEscape=true  [x] // captureBoxNonopaqueOwnedEscaping
//   opaque=false canGuarantee=false captureCanEscape=false [x] // captureBoxNonopaqueOwnedNonescaping
// - ImmutableBox // this has to be move-only, basically
//   - opaque? =true:unimplemented. noncopyable struct 'S' cannot conform to 'P', noncopyable type 'S' cannot be used with generics yet ( captureImmutableBoxOpaqueGuaranteedEscaping )
//   - canGuarantee?
//   - captureCanEscape?
//   - !isPack! -- assert(!isPack())
//   opaque=false canGuarantee=true  captureCanEscape=true  [x] // captureImmutableBoxNonopaqueGuaranteedEscaping
//   opaque=false canGuarantee=false captureCanEscape=true  [x] // captureImmutableBoxNonopaqueOwnedEscaping
//   opaque=false canGuarantee=false captureCanEscape=false [-] // captureImmutableBoxNonopaqueOwnedNonescaping // rdar://114207460
// - StorageAddress [x] captureStorageAddress
//   - isMoveOnlyWrapped? =true:unimplemented. noncopyable struct 'MO' must be @frozen in library evolution mode; non-@frozen public and @usableFromInline noncopyable types are not supported
//   - isPack?
//   isPack=true  [-] CRASH
//   isPack=false [x] captureStorageAddress
// - Constant // REQUIRES NOT-ADDRESS-ONLY!
// - Immutable
//   - canGuarantee?
//   - isPack?
//   canGuarantee=true  isPack=true  [ ] // captureImmutablePackGuaranteed
//   canGuarantee=true  isPack=false [x] // captureImmutableGuaranteed
//   canGuarantee=false isPack=true  [ ] // captureImmutableOwnedPack
//   canGuarantee=false isPack=false [x] // captureImmutableOwned

protocol P {}
struct PImpl : P {}
class C {}
struct G<T> {}
struct MOG<T> : ~Copyable {}
func getMOG<T>(_ t: T.Type) -> MOG<T> { return MOG<T>() }
func borrow<T>(_ t: T) {}
@propertyWrapper struct BoxWrapper<T> { var wrappedValue: T }

// CaptureKind::Immutable, canGuarantee=true, isPack=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures30captureImmutablePackGuaranteed1tyxxQp_tRvzlF6$deferL_yyRvzlF : {{.*}} {
// CHECK:       bb0(%0 : @closureCapture
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures30captureImmutablePackGuaranteed1tyxxQp_tRvzlF6$deferL_yyRvzlF'
func captureImmutablePackGuaranteed<each T>(t: repeat each T) {
  defer { (repeat borrow(each t)) }
}

// CaptureKind::Immutable, canGuarantee=true, isPack=false
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures26captureImmutableGuaranteed1tyx_tlF : {{.*}} {
// CHECK:       bb0([[INSTANCE:%[^,]+]] :
// CHECK:         [[STACK:%[^,]+]] = alloc_stack [lexical] [var_decl] $T, let, name "x"
// CHECK:         [[VAR:%[^,]+]] = mark_uninitialized [var] [[STACK]]
// CHECK:         [[COPY:%[^,]+]] = copy_value [[INSTANCE]]
// CHECK:         assign [[COPY]] to [[VAR]]
// CHECK:         [[LOAD:%[^,]+]] = load_borrow [[VAR]]
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures26captureImmutableGuaranteed1tyx_tlF6$deferL_yylF
// CHECK:         apply [[CLOSURE]]<T>([[LOAD]])
// CHECK:         end_borrow [[LOAD]]
// CHECK:         destroy_addr [[VAR]]
// CHECK:         dealloc_stack [[STACK]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures26captureImmutableGuaranteed1tyx_tlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures26captureImmutableGuaranteed1tyx_tlF6$deferL_yylF : {{.*}} {
// CHECK:       bb0([[INSTANCE:%[^,]+]] : @closureCapture @guaranteed $T):
// CHECK:         [[BORROW:%[^,]+]] = function_ref @$s22opaque_values_closures6borrowyyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> () 
// CHECK:         apply [[BORROW]]<T>([[INSTANCE]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures26captureImmutableGuaranteed1tyx_tlF6$deferL_yylF'
func captureImmutableGuaranteed<T>(t: T) {
  let x: T
  defer { borrow(x) }
  x = t
}

// CaptureKind::Immutable, canGuarantee=false, isPack=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures25captureImmutableOwnedPackyyxxQpRvzlF : {{.*}} {
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures25captureImmutableOwnedPackyyxxQpRvzlFyyXEfU_
// CHECK:         [[STACK_TUPLE:%[^,]+]] = alloc_stack $(repeat each T)
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]<Pack{repeat each T}>([[STACK_TUPLE]])
// CHECK:         [[NOESCAPE_CONTEXT:%[^,]+]] = convert_escape_to_noescape [not_guaranteed] [[CONTEXT]]
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures32captureImmutableOwnedPack_calleeyyyyXEF
// CHECK:         apply [[CALLEE]]([[NOESCAPE_CONTEXT]])
// CHECK:         destroy_value [[NOESCAPE_CONTEXT]]
// CHECK:         destroy_value [[CONTEXT]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures25captureImmutableOwnedPackyyxxQpRvzlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures25captureImmutableOwnedPackyyxxQpRvzlFyyXEfU_ : $@convention(thin) <each T> (@in_guaranteed (repeat each T)) -> () {
// CHECK:       bb0(%0 : @closureCapture $*(repeat each T)):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures25captureImmutableOwnedPackyyxxQpRvzlFyyXEfU_'
func captureImmutableOwnedPack<each T>(_ ts: repeat each T) {
  captureImmutableOwnedPack_callee {
    _ = (repeat each ts)
  }
}
func captureImmutableOwnedPack_callee(_ cl: () -> ()) {}

// CaptureKind::Immutable, canGuarantee=false, isPack=false
// CHECK-LABEL: sil hidden [ossa] @$s22opaque_values_closures21captureImmutableOwnedyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK:       bb0([[INSTANCE:%[^,]+]] :
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures21captureImmutableOwnedyyxlFyyXEfU_
// CHECK:         [[COPY:%[^,]+]] = copy_value [[INSTANCE]]
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]<T>([[COPY]])
// CHECK:         [[NOESCAPE_CONTEXT:%[^,]+]] = convert_escape_to_noescape [not_guaranteed] [[CONTEXT]]
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures28captureImmutableOwned_calleeyyyyXEF
// CHECK:         apply [[CALLEE]]([[NOESCAPE_CONTEXT]])
// CHECK:         destroy_value [[NOESCAPE_CONTEXT]]
// CHECK:         destroy_value [[CONTEXT]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures21captureImmutableOwnedyyxlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures21captureImmutableOwnedyyxlFyyXEfU_ : {{.*}} {
// CHECK:       bb0(%0 : @closureCapture @guaranteed $T):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures21captureImmutableOwnedyyxlFyyXEfU_'
func captureImmutableOwned<T>(_ t: T) {
  captureImmutableOwned_callee {
    _ = t
  }
}
func captureImmutableOwned_callee(_ cl: () -> ()) {}

// CaptureKind::StorageAddress, isMoveOnlyWrapped=false, isPack=false
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures21captureStorageAddressyyAA1GVyxGzlF : {{.*}} {
// CHECK:       bb0([[ADDR:%[^,]+]] :
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures21captureStorageAddressyyAA1GVyxGzlFyAEXEfU_
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]<T>([[ADDR]])
// CHECK:         [[CONVERTED_CONTEXT:%[^,]+]] = convert_function [[CONTEXT]]
// CHECK:         [[NOESCAPE_CONTEXT:%[^,]+]] = convert_escape_to_noescape [not_guaranteed] [[CONVERTED_CONTEXT]]
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures28captureStorageAddress_calleeyyyAA1GVyxGXElF
// CHECK:         apply [[CALLEE]]<T>([[NOESCAPE_CONTEXT]])
// CHECK:         destroy_value [[NOESCAPE_CONTEXT]]
// CHECK:         destroy_value [[CONVERTED_CONTEXT]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures21captureStorageAddressyyAA1GVyxGzlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures21captureStorageAddressyyAA1GVyxGzlFyAEXEfU_ : {{.*}} {
// CHECK:       bb0(%0 : {{.*}}, 
// CHECK:           %1 : @closureCapture $*G<T>):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures21captureStorageAddressyyAA1GVyxGzlFyAEXEfU_'
func captureStorageAddress<T>(_ g: inout G<T>) {
  captureStorageAddress_callee { o in
    g = o
  }
}
func captureStorageAddress_callee<T>(_ t: (G<T>) -> ()) {}

//    // Unimplemented: noncopyable struct 'S' cannot conform to 'P', noncopyable type 'S' cannot be used with generics yet ( captureImmutableBoxOpaqueGuaranteedEscaping )
//    // CaptureKind::ImmutableBox, opaque, canGuarantee=true, captureCanEscape=true
//    struct MOS : ~Copyable {}
//    func captureImmutableBoxOpaqueGuaranteedEscaping_vend() -> some Any { return MOS() }
//    func captureImmutableBoxOpaqueGuaranteedEscaping() {
//      var instance = captureImmutableBoxOpaqueGuaranteedEscaping_vend()
//      func local<T>(_ t: T) {
//        instance = captureImmutableBoxOpaqueGuaranteedEscaping_vend()
//      }
//      local(instance)
//    }

// CaptureKind::ImmutableBox, non-opaque, canGuarantee=true, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures46captureImmutableBoxNonopaqueGuaranteedEscapingyyxmlF : {{.*}} {
// CHECK:       bb0([[T:%[^,]+]] :
// CHECK:         [[BOX:%[^,]+]] = alloc_box ${ var TakingMOG }, var
// CHECK:         [[BOX_LIFETIME:%[^,]+]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[BOX_ADDR:%[^,]+]] = project_box [[BOX_LIFETIME]]
// CHECK:         mark_function_escape [[BOX_ADDR]]
// CHECK:         [[LOCAL:%[^,]+]] = function_ref @$s22opaque_values_closures46captureImmutableBoxNonopaqueGuaranteedEscapingyyxmlF5localL_yylF
// CHECK:         apply [[LOCAL]]<T>([[BOX_LIFETIME]], [[T]])
// CHECK:         end_borrow [[BOX_LIFETIME]]
// CHECK:         destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures46captureImmutableBoxNonopaqueGuaranteedEscapingyyxmlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures46captureImmutableBoxNonopaqueGuaranteedEscapingyyxmlF5localL_yylF : {{.*}} {
// CHECK:       bb0(%0 : @closureCapture @guaranteed ${ var TakingMOG },
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures46captureImmutableBoxNonopaqueGuaranteedEscapingyyxmlF5localL_yylF'
func captureImmutableBoxNonopaqueGuaranteedEscaping<T>(_ t: T.Type) {
  var us: TakingMOG = TakingMOG()

  func local() {
    us.takeMOG(getMOG(t))
  }
  local()
}
struct TakingMOG {
  mutating func takeMOG<T>(_ m: consuming MOG<T>) {}
}

// CaptureKind::ImmutableBox, non-opaque, canGuarantee=false, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures41captureImmutableBoxNonopaqueOwnedEscapingyyxmlF : {{.*}} {
// CHECK:         [[IMMUTABLEBOX:%[^,]+]] = alloc_box $<τ_0_0> { let MOG<τ_0_0> } <T>, let
// CHECK:         [[IMMUTABLEBOX_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[IMMUTABLEBOX]]
// CHECK:         [[IMMUTABLEBOX_ADDR:%[^,]+]] = project_box [[IMMUTABLEBOX_LIFETIME]]
// CHECK:         store {{%[^,]+}} to [init] [[IMMUTABLEBOX_ADDR]]
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures41captureImmutableBoxNonopaqueOwnedEscapingyyxmlFyycfU_ : $@convention(thin) <τ_0_0> (@guaranteed <τ_0_0> { let MOG<τ_0_0> } <τ_0_0>) -> () 
// CHECK:         [[IMMUTABLEBOX_COPY:%[^,]+]] = copy_value [[IMMUTABLEBOX_LIFETIME]]
// CHECK:         mark_function_escape [[IMMUTABLEBOX_ADDR]]
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]<T>([[IMMUTABLEBOX_COPY]])
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures48captureImmutableBoxNonopaqueOwnedEscaping_calleeyyyycF
// CHECK:         apply [[CALLEE]]([[CONTEXT]])
// CHECK:         destroy_value [[CONTEXT]] : $@callee_guaranteed () -> () 
// CHECK:         end_borrow [[IMMUTABLEBOX_LIFETIME]]
// CHECK:         destroy_value [[IMMUTABLEBOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures41captureImmutableBoxNonopaqueOwnedEscapingyyxmlF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures41captureImmutableBoxNonopaqueOwnedEscapingyyxmlFyycfU_ : {{.*}} {
// CHECK:       bb0(%0 : @closureCapture @guaranteed $<τ_0_0> { let MOG<τ_0_0> } <T>):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures41captureImmutableBoxNonopaqueOwnedEscapingyyxmlFyycfU_'
func captureImmutableBoxNonopaqueOwnedEscaping<T>(_ t: T.Type) {
  let g = getMOG(t)
  captureImmutableBoxNonopaqueOwnedEscaping_callee { _ = g }
}
func captureImmutableBoxNonopaqueOwnedEscaping_callee(_ t: @escaping () -> ()) {}

//    // rdar://114207460
//    // CaptureKind::ImmutableBox, non-opaque, canGuarantee=false, captureCanEscape=false
//    @propertyWrapper struct MOGWrapper<T> : ~Copyable { var wrappedValue: MOG<T> }
//    func captureImmutableBoxNonopaqueOwnedNonescaping<U>(_ get: () -> MOG<U>) {
//      @MOGWrapper<U> var u: MOG<U>
//    
//      func local() {
//        u = get()
//      }
//      local()
//    }

// CaptureKind::Box, opaque, canGuarantee=true, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures34captureBoxOpaqueGuaranteedEscapingyyF : {{.*}} {
// CHECK:         [[BOX:%[^,]+]] = alloc_box ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures39captureBoxOpaqueGuaranteedEscaping_vendQryF", 0) __ }, var
// CHECK:         [[BOX_LIFETIME:%[^,]+]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[BOX_ADDR:%[^,]+]] = project_box [[BOX_LIFETIME]]
// CHECK:         [[BOX2:%[^,]+]] = alloc_box ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures39captureBoxOpaqueGuaranteedEscaping_vendQryF", 0) __ } 
// CHECK:         [[BOX2_ADDR:%[^,]+]] = project_box [[BOX2]]
// CHECK:         copy_addr [[BOX_ADDR]] to [init] [[BOX2_ADDR]]
// CHECK:         [[BOX2_LIFETIME:%[^,]+]] = begin_borrow [[BOX2]]
// CHECK:         [[BOX_ACCESS:%[^,]+]] = begin_access [read] [unknown] [[BOX_ADDR]]
// CHECK:         [[INITIAL_VALUE:%[^,]+]] = load [trivial] [[BOX_ACCESS]]
// CHECK:         end_access [[BOX_ACCESS]]
// CHECK:         [[LOCAL:%[^,]+]] = function_ref @$s22opaque_values_closures34captureBoxOpaqueGuaranteedEscapingyyF5localL_yyxAA1PRzlF
// CHECK:         apply [[LOCAL]]<PImpl>([[INITIAL_VALUE]], [[BOX2_LIFETIME]])
// CHECK:         end_borrow [[BOX2_LIFETIME]]
// CHECK:         destroy_value [[BOX2]]
// CHECK:         destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures34captureBoxOpaqueGuaranteedEscapingyyF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures34captureBoxOpaqueGuaranteedEscapingyyF5localL_yyxAA1PRzlF : {{.*}} {
// CHECK:       bb0(%0 : @guaranteed $T, 
// CHECK-SAME:      %1 : @closureCapture @guaranteed ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures39captureBoxOpaqueGuaranteedEscaping_vendQryF", 0) __ }):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures34captureBoxOpaqueGuaranteedEscapingyyF5localL_yyxAA1PRzlF'
func captureBoxOpaqueGuaranteedEscaping() {
  var instance = captureBoxOpaqueGuaranteedEscaping_vend()
  func local<T : P>(_ t: T) {
    instance = captureBoxOpaqueGuaranteedEscaping_vend()
  }
  local(instance)
}
func captureBoxOpaqueGuaranteedEscaping_vend() -> some P { return PImpl() }

// CaptureKind::Box, opaque, canGuarantee=false, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures29captureBoxOpaqueOwnedEscapingyyF : {{.*}} {
// CHECK:         [[BOX:%[^,]+]] = alloc_box ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures34captureBoxOpaqueOwnedEscaping_vendQryF", 0) __ }, var
// CHECK:         [[BOX_LIFETIME:%[^,]+]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[BOX_ADDR:%[^,]+]] = project_box [[BOX_LIFETIME]]
// CHECK:         [[BOX_ACCESS:%[^,]+]] = begin_access [read] [unknown] [[BOX_ADDR]]
// CHECK:         [[FIRST:%[^,]+]] = load [trivial] [[BOX_ACCESS]]
// CHECK:         end_access [[BOX_ACCESS]]
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures29captureBoxOpaqueOwnedEscapingyyFyAA0defgH5_vendQryFQOyQo_cfU_
// CHECK:         [[BOX2:%[^,]+]] = alloc_box ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures34captureBoxOpaqueOwnedEscaping_vendQryF", 0) __ } 
// CHECK:         [[BOX2_ADDR:%[^,]+]] = project_box [[BOX2]]
// CHECK:         copy_addr [[BOX_ADDR]] to [init] [[BOX2_ADDR]]
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]([[BOX2]])
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures36captureBoxOpaqueOwnedEscaping_calleeyyx_yxctAA1PRzlF
// CHECK:         apply [[CALLEE]]<PImpl>([[FIRST]], [[CONTEXT]])
// CHECK:         destroy_value [[CONTEXT]]
// CHECK:         destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures29captureBoxOpaqueOwnedEscapingyyF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures29captureBoxOpaqueOwnedEscapingyyFyAA0defgH5_vendQryFQOyQo_cfU_ : {{.*}} {
// CHECK:       bb0(%0 : $PImpl, 
// CHECK-SAME:      %1 : @closureCapture @guaranteed ${ var @_opaqueReturnTypeOf("$s22opaque_values_closures34captureBoxOpaqueOwnedEscaping_vendQryF", 0) __ }):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures29captureBoxOpaqueOwnedEscapingyyFyAA0defgH5_vendQryFQOyQo_cfU_'
func captureBoxOpaqueOwnedEscaping() {
  var instance = captureBoxOpaqueOwnedEscaping_vend()
  captureBoxOpaqueOwnedEscaping_callee(instance) { new in
    instance = new
  }
}
func captureBoxOpaqueOwnedEscaping_vend() -> some P { return PImpl() }
func captureBoxOpaqueOwnedEscaping_callee<T : P>(_ t: T, _ c: @escaping (T) -> ()) {}

//    // rdar://114195999
//    // CaptureKind::Box, opaque, canGuarantee=false, captureCanEscape=false
//    protocol Q {
//      associatedtype A
//      static func vend() -> A
//    }
//    struct PImpl : P {}
//    struct QI : Q {
//      static func vend() -> some P { return PImpl() }
//    }
//    @propertyWrapper struct BoxWrapper<T> { var wrappedValue: T }
//    func captureBoxOpaqueOwnedNonescaping() {
//      @BoxWrapper var u: QI.A
//    
//      func local() {
//        u = QI.vend()
//      }
//      local()
//    }

// CaptureKind::Box, non-opaque, canGuarantee=true, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures37captureBoxNonopaqueGuaranteedEscapingyyxyXElF : {{.*}} {
// CHECK:       bb0([[GET:%[^,]+]] :
// CHECK:         [[BOX:%[^,]+]] = alloc_box $<τ_0_0> { var Array<τ_0_0> } <U>, var
// CHECK:         [[BOX_LIFETIME:%[^,]+]] = begin_borrow [var_decl] [[BOX]]
// CHECK:         [[BOX_ADDR:%[^,]+]] = project_box [[BOX_LIFETIME]]
// CHECK:         mark_function_escape [[BOX_ADDR]]
// CHECK:         [[LOCAL:%[^,]+]] = function_ref @$s22opaque_values_closures37captureBoxNonopaqueGuaranteedEscapingyyxyXElF5localL_yylF
// CHECK:         apply [[LOCAL]]<U>([[BOX_LIFETIME]], [[GET]])
// CHECK:         end_borrow [[BOX_LIFETIME]]
// CHECK:         destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures37captureBoxNonopaqueGuaranteedEscapingyyxyXElF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures37captureBoxNonopaqueGuaranteedEscapingyyxyXElF5localL_yylF : {{.*}} {
// CHECK:       bb0(%0 : @closureCapture @guaranteed $<τ_0_0> { var Array<τ_0_0> } <U>, 
// CHECK:           %1 :
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures37captureBoxNonopaqueGuaranteedEscapingyyxyXElF5localL_yylF'
func captureBoxNonopaqueGuaranteedEscaping<U>(_ get: () -> U) {
  var us: [U] = []

  func local() {
    us.append(get())
  }
  local()
}

// CaptureKind::Box, non-opaque, canGuarantee=false, captureCanEscape=true
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures32captureBoxNonopaqueOwnedEscapingyyF : {{.*}} {
// CHECK:         [[BOX:%[^,]+]] = alloc_box ${ var C }, var
// CHECK:         [[BOX_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:         [[BOX_ADDR:%[^,]+]] = project_box [[BOX_LIFETIME]]
// CHECK:         [[BOX_ACCESS:%[^,]+]] = begin_access [read] [unknown] [[BOX_ADDR]]
// CHECK:         [[INITIAL_VALUE:%[^,]+]] = load [copy] [[BOX_ACCESS]]
// CHECK:         end_access [[BOX_ACCESS]]
// CHECK:         [[CLOSURE:%[^,]+]] = function_ref @$s22opaque_values_closures32captureBoxNonopaqueOwnedEscapingyyFyAA1CCcfU_
// CHECK:         [[BOX_COPY:%[^,]+]] = copy_value [[BOX_LIFETIME]]
// CHECK:         mark_function_escape [[BOX_ADDR]]
// CHECK:         [[CONTEXT:%[^,]+]] = partial_apply [callee_guaranteed] [[CLOSURE]]([[BOX_COPY]])
// CHECK:         [[CALLEE:%[^,]+]] = function_ref @$s22opaque_values_closures39captureBoxNonopaqueOwnedEscaping_calleeyyx_yxctlF
// CHECK:         apply [[CALLEE]]<C>([[INITIAL_VALUE]], [[CONTEXT]])
// CHECK:         destroy_value [[CONTEXT]]
// CHECK:         destroy_value [[INITIAL_VALUE]]
// CHECK:         end_borrow [[BOX_LIFETIME]]
// CHECK:         destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures32captureBoxNonopaqueOwnedEscapingyyF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures32captureBoxNonopaqueOwnedEscapingyyFyAA1CCcfU_ : {{.*}} {
// CHECK:       bb0(%0 : {{.*}}, 
// CHECK:           %1 : @closureCapture @guaranteed ${ var C }):
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures32captureBoxNonopaqueOwnedEscapingyyFyAA1CCcfU_'
func captureBoxNonopaqueOwnedEscaping() {
  var instance = captureBoxNonopaqueOwnedEscaping_vend()
  captureBoxNonopaqueOwnedEscaping_callee(instance) { new in
    instance = new
  }
}
func captureBoxNonopaqueOwnedEscaping_callee<T>(_ t: T, _ c: @escaping (T) -> ()) {}
func captureBoxNonopaqueOwnedEscaping_vend() -> C { return C() }

// CaptureKind::Box, non-opaque, canGuarantee=false, captureCanEscape=false
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures35captureBoxNonopaqueOwnedNonescapingyyxyXElF : {{.*}} {
// CHECK:       bb0([[GET:%[^,]+]] :
// CHECK:         [[BOX:%[^,]+]] = alloc_box $<τ_0_0> { var BoxWrapper<τ_0_0> } <U>, var
// CHECK:         [[VAR:%[^,]+]] = mark_uninitialized [var] [[BOX]]
// CHECK:         [[VAR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VAR]]
// CHECK:         [[VAR_ADDR:%[^,]+]] = project_box [[VAR_LIFETIME]]
// TODO: DETERMINE: Considering that captureCanEscape is false, should this mark_function_escape be emitted?
// CHECK:         mark_function_escape [[VAR_ADDR]]
// CHECK:         [[LOCAL:%[^,]+]] = function_ref @$s22opaque_values_closures35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF
// CHECK:         apply [[LOCAL]]<U>([[VAR_LIFETIME]], [[GET]])
// CHECK:         end_borrow [[VAR_LIFETIME]]
// CHECK:         destroy_value [[VAR]]
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures35captureBoxNonopaqueOwnedNonescapingyyxyXElF'
// CHECK-LABEL: sil {{.*}}[ossa] @$s22opaque_values_closures35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF : {{.*}}
// CHECK:       bb0(%0 : @closureCapture @guaranteed $<τ_0_0> { var BoxWrapper<τ_0_0> } <U>, 
// CHECK-SAME:      %1 :
// CHECK-LABEL: } // end sil function '$s22opaque_values_closures35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF'
func captureBoxNonopaqueOwnedNonescaping<U>(_ get: () -> U) {
  @BoxWrapper var u: U

  func local() {
    u = get()
  }
  local()
}

