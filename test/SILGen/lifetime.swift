// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

struct Buh<T> { var x:Int { get: set: } }

class Ref {
}
struct Val {
}

// CHECK: sil @_T8lifetime13local_valtypeFT_T_
func local_valtype() {
    var b : Val
    // CHECK: [[B:%[0-9]+]] = alloc_box $Val
    // CHECK: release [[B]]
    // CHECK: return
}

// CHECK: sil @_T8lifetime20local_valtype_branchFT1aSb_T_
func local_valtype_branch(a:Bool) {
    // CHECK: [[A:%[0-9]+]] = alloc_box $Bool

    if a { return }
    // CHECK: condbranch
    // CHECK: {{bb.*:}}
    // CHECK: br [[EPILOG:bb[0-9]+]]

    var x:Int
    // CHECK: [[X:%[0-9]+]] = alloc_box $Int

    if a { return }
    // CHECK: condbranch
    // CHECK: {{bb.*:}}
    // CHECK: release [[X]]
    // CHECK: br [[EPILOG]]

    while a {
    // CHECK: condbranch
        if a { break }
        // CHECK: condbranch
        // CHECK: {{bb.*:}}
        // CHECK-NOT: release [[X]]
        // CHECK: br

        if a { return }
        // CHECK: condbranch
        // CHECK: {{bb.*:}}
        // CHECK: release [[X]]
        // CHECK: br [[EPILOG]]

        var y:Int
        // CHECK: [[Y:%[0-9]+]] = alloc_box $Int

        if a { break }
        // CHECK: condbranch
        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK-NOT: release [[X]]
        // CHECK-NOT: release [[A]]
        // CHECK: br

        if a { return }
        // CHECK: condbranch
        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK: release [[X]]
        // CHECK: br [[EPILOG]]

        if true {
            var z:Int
            // CHECK: [[Z:%[0-9]+]] = alloc_box $Int

            if a { break }
            // CHECK: condbranch
            // CHECK: {{bb.*:}}
            // CHECK: release [[Z]]
            // CHECK: release [[Y]]
            // CHECK-NOT: release [[X]]
            // CHECK-NOT: release [[A]]
            // CHECK: br

            if a { return }
            // CHECK: condbranch
            // CHECK: {{bb.*:}}
            // CHECK: release [[Z]]
            // CHECK: release [[Y]]
            // CHECK: release [[X]]
            // CHECK: br [[EPILOG]]

            // CHECK: release [[Z]]
        }
        if a { break }
        // CHECK: condbranch
        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK-NOT: release [[X]]
        // CHECK-NOT: release [[A]]
        // CHECK: br

        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK: br
    }
    // CHECK: release [[X]]
    // CHECK: [[EPILOG]]:
    // CHECK: release [[A]]
    // CHECK: return
}

func reftype_func() -> Ref {}
func reftype_func_with_arg(x:Ref) -> Ref {}

// CHECK: sil @_T8lifetime14reftype_returnFT_CS_3Ref
func reftype_return() -> Ref {
    return reftype_func()
    // CHECK: [[RF:%[0-9]+]] = function_ref @_T8lifetime12reftype_funcFT_CS_3Ref : $[thin] () -> Ref
    // CHECK-NOT: release
    // CHECK: [[RET:%[0-9]+]] = apply [[RF]]()
    // CHECK-NOT: release
    // CHECK: return [[RET]]
}

// CHECK: sil @_T8lifetime11reftype_argFT1aCS_3Ref_T_
func reftype_arg(a:Ref) {
    // CHECK: bb0([[A:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK-NOT: retain [[A]]
    // CHECK: store [[A]] to [[AADDR]]
    // CHECK: release [[AADDR]]
    // CHECK: return
}

// CHECK: sil @_T8lifetime17reftype_byref_argFT1aRCS_3Ref_T_
func reftype_byref_arg(a:[byref] Ref) {
    // CHECK: bb0([[A:%[0-9]+]] : $*Ref):
    // CHECK-NOT: retain
    // CHECK-NOT: release
    // CHECK: return
}

// CHECK: sil @_T8lifetime26reftype_call_ignore_returnFT_T_
func reftype_call_ignore_return() {
    reftype_func()
    // CHECK: = function_ref @_T8lifetime12reftype_funcFT_CS_3Ref : $[thin] () -> Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK: release [[R]]
    // CHECK: return
}

// CHECK: sil @_T8lifetime27reftype_call_store_to_localFT_T_
func reftype_call_store_to_local() {
    var a = reftype_func()
    // CHECK: [[A:%[0-9]+]] = alloc_box $Ref
    // CHECK: = function_ref @_T8lifetime12reftype_funcFT_CS_3Ref : $[thin] () -> Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK-NOT: retain [[R]]
    // CHECK: store [[R]] to [[A]]
    // CHECK-NOT: release [[R]]
    // CHECK: release [[A]]
    // CHECK-NOT: release [[R]]
    // CHECK: return
}

// CHECK: sil @_T8lifetime16reftype_call_argFT_T_
func reftype_call_arg() {
    reftype_func_with_arg(reftype_func())
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @_T8lifetime21reftype_func_with_argFT1xCS_3Ref_S0_ : $[thin] (x : Ref) -> Ref
    // CHECK: [[RF:%[0-9]+]] = function_ref @_T8lifetime12reftype_funcFT_CS_3Ref : $[thin] () -> Ref
    // CHECK: [[R1:%[0-9]+]] = apply [[RF]]
    // CHECK: [[R2:%[0-9]+]] = apply [[RFWA]]([[R1]])
    // CHECK: release [[R2]]
    // CHECK: return
}

// CHECK: sil @_T8lifetime21reftype_call_with_argFT1aCS_3Ref_T_
func reftype_call_with_arg(a:Ref) {
    // CHECK: bb0([[A1:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK: store [[A1]] to [[AADDR]]

    reftype_func_with_arg(a)
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @_T8lifetime21reftype_func_with_argFT1xCS_3Ref_S0_ : $[thin] (x : Ref) -> Ref
    // CHECK: [[A2:%[0-9]+]] = load [[AADDR]]
    // CHECK: retain [[A2]]
    // CHECK: = apply [[RFWA]]([[A2]])

    // CHECK: return
}

// CHECK: sil @_T8lifetime16reftype_reassignFT1aRCS_3Ref1bS0__T_
func reftype_reassign(a:[byref] Ref, b:Ref) {
    // CHECK: bb0([[AADDR:%[0-9]+]] : $*Ref, [[B1:%[0-9]+]] : $Ref):
    // CHECK: [[BADDR:%[0-9]+]] = alloc_box $Ref
    a = b
    // CHECK: [[B2:%[0-9]+]] = load [[BADDR]]
    // CHECK: retain [[B2]]
    // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
    // CHECK: store [[B2]] to [[AADDR]]
    // CHECK: release [[A]]

    // CHECK: return
}

func tuple_with_ref_elements() -> (Val, (Ref, Val), Ref) {}

// CHECK: sil @_T8lifetime28tuple_with_ref_ignore_returnFT_T_
func tuple_with_ref_ignore_return() {
  tuple_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T8lifetime23tuple_with_ref_elementsFT_TVS_3ValTCS_3RefS0__S1__
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: [[T1_0:%[0-9]+]] = tuple_extract [[T1]] : {{.*}}, 0
  // CHECK: [[T2:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 2
  // CHECK: release [[T2]]
  // CHECK: release [[T1_0]]
  // CHECK: return
}

struct Aleph {
  var a:Ref
  var b:Val

  // -- loadable value constructor:
  // CHECK: sil @_TV8lifetime5AlephCfMS0_FT1aCS_3Ref1bVS_3Val_S0_ : $[thin] ((a : Ref, b : Val), Aleph.metatype) -> Aleph
  // CHECK-NEXT: bb0([[A:%.*]] : $Ref, [[B:%.*]] : $Val, {{%.*}} : $Aleph.metatype):
  // CHECK-NEXT:   [[RET:%.*]] = struct $Aleph ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]

  // -- implicit default constructor:
  // CHECK: sil @_TV8lifetime5AlephCfMS0_FT_S0_ : $[thin] ((), Aleph.metatype) -> Aleph
  // CHECK: bb0({{%.*}} : $Aleph.metatype):
  // CHECK:   [[THIS:%.*]] = alloc_box $Aleph
  // CHECK:   initialize_var [no_default_construct] [[THIS]]
  // -- a (FIXME: shouldn't be default constructible)
  // CHECK:   [[A_DEFAULT:%.*]] = builtin_zero $Ref
  // CHECK:   [[THIS_A:%.*]] = struct_element_addr {{.*}}, #a
  // CHECK:   store [[A_DEFAULT]] to [[THIS_A]]
  // -- b
  // CHECK:   [[B_DEFAULT_CTOR:%.*]] = function_ref @_TV8lifetime3ValCfMS0_FT_S0_ : $[thin] ((), Val.metatype) -> Val
  // CHECK:   [[B_DEFAULT:%.*]] = apply [[B_DEFAULT_CTOR]]({{%.*}})
  // CHECK:   [[THIS_B:%.*]] = struct_element_addr {{.*}}, #b
  // CHECK:   store [[B_DEFAULT]] to [[THIS_B]]
}

struct Beth {
  var a:Val
  var b:Aleph
  var c:Ref

  func gimel() {}
}

protocol Unloadable {}

struct Daleth {
  var a:Aleph
  var b:Beth
  var c:Unloadable

  // -- address-only value constructor:
  // CHECK: sil @_TV8lifetime6DalethCfMS0_FT1aVS_5Aleph1bVS_4Beth1cPS_10Unloadable__S0_ : $[thin] ((a : Aleph, b : Beth, c : Unloadable), Daleth.metatype) -> Daleth {
  // CHECK: bb0([[THIS:%.*]] : $*Daleth, [[A:%.*]] : $Aleph, [[B:%.*]] : $Beth, [[C:%.*]] : $*Unloadable, {{%.*}} : $Daleth.metatype):
  // CHECK-NEXT:   [[A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #a
  // CHECK-NEXT:   store [[A]] to [[A_ADDR]]
  // CHECK-NEXT:   [[B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #b
  // CHECK-NEXT:   store [[B]] to [[B_ADDR]]
  // CHECK-NEXT:   [[C_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #c
  // CHECK-NEXT:   copy_addr [take] [[C]] to [initialization] [[C_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

class He {
  // -- implicit default allocator:
  // CHECK: sil @_TC8lifetime2HeCfMS0_FT_S0_ : $[thin] ((), He.metatype) -> He {
  // CHECK-NEXT: bb0({{%.*}} : $He.metatype):
  // CHECK-NEXT:   [[THIS:%.*]] = alloc_ref $He
  // CHECK-NEXT:   [[INIT:%.*]] = function_ref @_TC8lifetime2HecfMS0_FT_S0_ : $[cc(method), thin] ((), He) -> He
  // CHECK-NEXT:   [[THIS1:%.*]] = apply [[INIT]]([[THIS]])
  // CHECK-NEXT:   return [[THIS1]]
  // CHECK-NEXT: }

  // -- implicit default initializer:
  // CHECK: sil @_TC8lifetime2HecfMS0_FT_S0_ : $[cc(method), thin] ((), He) -> He {
  // CHECK: bb0([[THIS:%.*]] : $He):
  // CHECK:   return [[THIS]]
  // CHECK: }
}

struct Waw {
  var a:(Ref, Val)
  var b:Val

  // -- loadable value constructor with tuple destructuring:
  // CHECK: sil @_TV8lifetime3WawCfMS0_FT1aTCS_3RefVS_3Val_1bS2__S0_ : $[thin] ((a : (Ref, Val), b : Val), Waw.metatype) -> Waw 
  // CHECK-NEXT: bb0([[A0:%.*]] : $Ref, [[A1:%.*]] : $Val, [[B:%.*]] : $Val, {{%.*}} : $Waw.metatype):
  // CHECK-NEXT:   [[A:%.*]] = tuple ([[A0]] : {{.*}}, [[A1]] : {{.*}})
  // CHECK-NEXT:   [[RET:%.*]] = struct $Waw ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]
}

struct Zayin {
  var a:(Unloadable, Val)
  var b:Unloadable

  // -- address-only value constructor with tuple destructuring:
  // CHECK: sil @_TV8lifetime5ZayinCfMS0_FT1aTPS_10Unloadable_VS_3Val_1bPS1___S0_ : $[thin] ((a : (Unloadable, Val), b : Unloadable), Zayin.metatype) -> Zayin
  // CHECK-NEXT: bb0([[THIS:%.*]] : $*Zayin, [[A0:%.*]] : $*Unloadable, [[A1:%.*]] : $Val, [[B:%.*]] : $*Unloadable, {{%.*}} : $Zayin.metatype):
  // CHECK-NEXT:   [[THIS_A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #a
  // CHECK-NEXT:   [[THIS_A0_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 0
  // CHECK-NEXT:   [[THIS_A1_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 1
  // CHECK-NEXT:   copy_addr [take] [[A0]] to [initialization] [[THIS_A0_ADDR]]
  // CHECK-NEXT:   store [[A1]] to [[THIS_A1_ADDR]]
  // CHECK-NEXT:   [[THIS_B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #b
  // CHECK-NEXT:   copy_addr [take] [[B]] to [initialization] [[THIS_B_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

func fragile_struct_with_ref_elements() -> Beth {}

// CHECK: sil @_T8lifetime29struct_with_ref_ignore_returnFT_T_
func struct_with_ref_ignore_return() {
  fragile_struct_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T8lifetime32fragile_struct_with_ref_elementsFT_VS_4Beth
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[S1:%[0-9]+]] = struct_extract [[STRUCT]] : $Beth, #b
  // CHECK: [[S1_0:%[0-9]+]] = struct_extract [[S1]] : $Aleph, #a
  // CHECK: release [[S1_0]]
  // CHECK: [[S2:%[0-9]+]] = struct_extract [[STRUCT]] : $Beth, #c
  // CHECK: release [[S2]]
  // CHECK: return
}

// CHECK: sil @_T8lifetime28struct_with_ref_materializedFT_T_
func struct_with_ref_materialized() {
  fragile_struct_with_ref_elements().gimel()
  // CHECK: [[METHOD:%[0-9]+]] = function_ref @_TV8lifetime4Beth5gimelfRS0_FT_T_
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T8lifetime32fragile_struct_with_ref_elementsFT_VS_4Beth
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[MATERIALIZED:%[0-9]+]] = alloc_stack $Beth
  // CHECK: store [[STRUCT]] to [[MATERIALIZED]]#1
  // CHECK: apply [[METHOD]]([[MATERIALIZED]]#1)
  // CHECK: [[STRUCT2:%[0-9]+]] = load [[MATERIALIZED]]#1
  // CHECK: [[S1:%[0-9]+]] = struct_extract [[STRUCT2]] : $Beth, #b
  // CHECK: [[S1_0:%[0-9]+]] = struct_extract [[S1]] : $Aleph, #a
  // CHECK: release [[S1_0]]
  // CHECK: [[S2:%[0-9]+]] = struct_extract [[STRUCT2]] : $Beth, #c
  // CHECK: release [[S2]]
  // CHECK: dealloc_stack [[MATERIALIZED]]#0
}

class RefWithProp {
  var int_prop:Int { get: set: }
  var aleph_prop:Aleph { get: set: }
}

// CHECK: sil @_T8lifetime23logical_lvalue_lifetimeFT1rCS_11RefWithProp1iSi1vVS_3Val_T_
func logical_lvalue_lifetime(r:RefWithProp, i:Int, v:Val) {
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box $RefWithProp
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val

  // -- Reference types need to be retained as property method args.
  r.int_prop = i
  // CHECK: [[R1:%[0-9]+]] = load [[RADDR]]
  // CHECK: retain [[R1]]
  // CHECK: retain [[R1]]
  // CHECK: [[SETTER_METHOD:%[0-9]+]] = function_ref @_TC8lifetime11RefWithProp8int_propSis
  // CHECK: apply [[SETTER_METHOD]]({{.*}}, [[R1]])
  // CHECK: release [[R1]]

  r.aleph_prop.b = v
  // CHECK: [[R2:%[0-9]+]] = load [[RADDR]]
  // CHECK: retain [[R2]]
  // CHECK: retain [[R2]]
  // CHECK: [[GETTER_METHOD:%[0-9]+]] = function_ref @_TC8lifetime11RefWithProp10aleph_propVS_5Alephg
  // CHECK: apply [[GETTER_METHOD]]([[R2]])
  // CHECK: [[ALEPH_PROP_MAT:%[0-9]+]] = alloc_stack $Aleph
  // CHECK: retain [[R2]]
  // CHECK: [[SETTER_METHOD:%[0-9]+]] = function_ref @_TC8lifetime11RefWithProp10aleph_propVS_5Alephs
  // CHECK: apply [[SETTER_METHOD]]({{.*}}, [[R2]])

  // -- Written-back materializes are consumed by the writeback operation
  // -- so they don't need to be loaded and released.
  // CHECK-NOT: load [[ALEPH_PROP_MAT]]
  // CHECK: release [[R2]]
}

func bar() -> Int {}

class Foo<T> {
  var x:Int
  var y:(Int, Ref)
  var z:T
  var w:Ref

  static func makeT() -> T {}

  // Class constructor
  constructor() {
  // -- allocating entry point
  // CHECK: sil @_TC8lifetime3FooCU__fMGS0_Q__FT_GS0_Q__ :
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $Foo<T>.metatype):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TC8lifetime3FoocU__fMGS0_Q__FT_GS0_Q__
    // CHECK: [[INIT_SPEC:%[0-9]+]] = specialize [[INIT_METHOD]] : {{.*}}, ${{.*}}
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_SPEC]]([[THIS]])
    // CHECK: return [[INIT_THIS]]

  // -- initializing entry point
  // CHECK: sil @_TC8lifetime3FoocU__fMGS0_Q__FT_GS0_Q__ :
    // CHECK: bb0([[THIS:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box $Foo<T>
    // CHECK: store [[THIS]] to [[THISADDR]]

    // initialization for y
    // CHECK: [[INTCTOR:%[0-9]+]] = function_ref @_TSiCfMSiFT_Si : $[thin] ((), Int64.metatype) -> Int64
    // CHECK: [[INTMETA:%[0-9]+]] = metatype $Int64.metatype
    // CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCTOR]]([[INTMETA]])

    x = bar()
    // CHECK: function_ref @_T8lifetime3barFT_Si : $[thin] () -> Int64
    // CHECK: [[THIS2:%[0-9]+]] = load [[THISADDR]]
    // CHECK: retain [[THIS2]]
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS2]] : {{.*}}, #x
    // CHECK: store {{.*}} to [[THIS_X]]
    // CHECK: release [[THIS2]]

    z = Foo<T>.makeT()
    // CHECK: [[FOOMETA:%[0-9]+]] = metatype $Foo<T>.metatype
    // CHECK: [[MAKET:%[0-9]+]] = class_method [[FOOMETA]] : {{.*}}, #Foo.makeT!1
    // CHECK: specialize [[MAKET]] : {{.*}}, $[thin] ((), Foo<T>.metatype) -> T, T = T
    // CHECK: ref_element_addr

    // -- cleanup this lvalue and return this
    // CHECK: [[THIS3:%[0-9]+]] = load [[THISADDR]]
    // CHECK: return [[THIS3]]
  }

  constructor(chi:Int) {
    z = Foo<T>.makeT()

  // -- allocating entry point
  // CHECK: sil @_TC8lifetime3FooCU__fMGS0_Q__FT3chiSi_GS0_Q__ :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int64, [[METATYPE:%[0-9]+]] : $Foo<T>.metatype):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TC8lifetime3FoocU__fMGS0_Q__FT3chiSi_GS0_Q__
    // CHECK: [[INIT_SPEC:%[0-9]+]] = specialize [[INIT_METHOD]] : {{.*}}, ${{.*}}
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_SPEC]]([[CHI]], [[THIS]])
    // CHECK: return [[INIT_THIS]]

  // -- initializing entry point
  // CHECK: sil @_TC8lifetime3FoocU__fMGS0_Q__FT3chiSi_GS0_Q__ :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int64, [[THIS:%[0-9]+]] : $Foo<T>):
    // CHECK: [[CHIADDR:%[0-9]+]] = alloc_box $Int64
    // CHECK: store [[CHI]] to [[CHIADDR]]
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box $Foo<T>
    // CHECK: store [[THIS]] to [[THISADDR]]

    x = chi
    // CHECK: [[CHI2:%[0-9]+]] = load [[CHIADDR]]#1 : $*Int64
    // CHECK: [[THIS2:%[0-9]+]] = load [[THISADDR]]#1 : $*Foo<T>
    // CHECK: retain [[THIS2]]
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS2]] : {{.*}}, #x
    // CHECK: store [[CHI2]] to [[THIS_X]]
    // CHECK: release [[THIS2]]

    // -- cleanup chi
    // CHECK: release [[CHIADDR]]
    // -- cleanup this lvalue
    // CHECK: [[THIS3:%[0-9]+]] = load [[THISADDR]]
    // CHECK: return [[THIS3]]
  }

  // -- allocating entry point
  // CHECK: sil @_TC8lifetime3FooC{{.*}} :
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TC8lifetime3Fooc{{.*}}
    // CHECK: specialize [[INIT_METHOD]] : {{.*}}, ${{.*}}, T = T, U = U
  // -- initializing entry point
  // CHECK: sil @_TC8lifetime3Fooc{{.*}} :
  constructor<U:Intifiable>(chi:U) {
    z = Foo<T>.makeT()

    x = chi.intify()
  }

  // CHECK-LABEL: sil @_TC8lifetime3Food : $[thin] <T> Foo<T> -> Builtin.ObjectPointer
  destructor {
    // CHECK: bb0([[THIS:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box $Foo<T>
    // CHECK: store [[THIS]] to [[THISADDR]]#1
    bar()
    // CHECK: function_ref @_T8lifetime3barFT_Si
    // CHECK: apply
    // CHECK: dealloc_box $Foo<T>, [[THISADDR]]#0

    // -- don't need to release x because it's trivial
    // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #x
    // -- release y
    // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #y
    // CHECK: destroy_addr [[YADDR]]
    // -- release z
    // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #z
    // CHECK: destroy_addr [[ZADDR]]
    // -- release w
    // CHECK: [[WADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #w
    // CHECK: destroy_addr [[WADDR]]
    // -- return back this
    // CHECK: [[PTR:%.*]] = ref_to_object_pointer [[THIS]] : ${{.*}} to $Builtin.ObjectPointer
    // CHECK: return [[PTR]]
  }
}

class ImplicitDtor {
  var x:Int
  var y:(Int, Ref)
  var w:Ref

  // CHECK-LABEL: sil @_TC8lifetime12ImplicitDtord
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtor):
  // -- don't need to release x because it's trivial
  // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #x
  // -- release y
  // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #y
  // CHECK: destroy_addr [[YADDR]]
  // -- release w
  // CHECK: [[WADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #w
  // CHECK: destroy_addr [[WADDR]]
  // CHECK: return
}

class ImplicitDtorDerived<T> : ImplicitDtor {
  var z:T

  // CHECK: sil @_TC8lifetime19ImplicitDtorDerivedCU__fMGS0_Q__FT_GS0_Q__
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerived<T>):
  // -- release z
  // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #z
  // CHECK: destroy_addr [[ZADDR]]
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtor
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @_TC8lifetime12ImplicitDtord
  // CHECK: apply [[BASE_DTOR]]([[BASE]])
}

class ImplicitDtorDerivedFromGeneric : ImplicitDtorDerived<Int> {
  // CHECK: sil @_TC8lifetime30ImplicitDtorDerivedFromGenericCfMS0_FT_S0_
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerivedFromGeneric):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtorDerived<Int64>
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @_TC8lifetime19ImplicitDtorDerivedd
  // CHECK: [[BASE_DTOR_SPEC:%[0-9]+]] = specialize [[BASE_DTOR]] : {{.*}}, $[thin] ImplicitDtorDerived<Int64> -> Builtin.ObjectPointer, T = Int
  // CHECK: [[PTR:%.*]] = apply [[BASE_DTOR_SPEC]]([[BASE]])
  // CHECK: return [[PTR]]
}

protocol Intifiable {
  func intify() -> Int
}

struct Bar {
  var x:Int

  // Loadable struct constructor
  // CHECK: sil @_TV8lifetime3BarC{{.*}}
  constructor() {
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $Bar.metatype):
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box $Bar
    // CHECK: initialize_var [no_default_construct] [[THISADDR]]

    x = bar()
    // CHECK: [[THIS_X:%[0-9]+]] = struct_element_addr [[THISADDR]]#1 : $*Bar, #x
    // CHECK: store {{.*}} to [[THIS_X]]

    // -- load and return this
    // CHECK: [[THISVAL:%[0-9]+]] = load [[THISADDR]]
    // CHECK: release [[THISADDR]]
    // CHECK: return [[THISVAL]]
  }

  constructor<T:Intifiable>(xx:T) {
    x = xx.intify()
  }
}

struct Bas<T> {
  var x:Int
  var y:T

  // Address-only struct constructor
  // CHECK: sil @_TV8lifetime3BasC{{.*}}
  constructor(yy:T) {
    // CHECK: bb0([[THISADDRPTR:%[0-9]+]] : $*Bas<T>, [[YYADDR:%[0-9]+]] : $*T, [[META:%[0-9]+]] : $Bas<T>.metatype):
    // CHECK: alloc_box
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box
    // CHECK: initialize_var [no_default_construct] [[THISADDR]]

    x = bar()
    // CHECK: [[THIS_X:%[0-9]+]] = struct_element_addr [[THISADDR]]#1 : $*Bas<T>, #x
    // CHECK: store {{.*}} to [[THIS_X]]

    y = yy
    // CHECK: [[THIS_Y:%[0-9]+]] = struct_element_addr [[THISADDR]]#1 : $*Bas<T>, #y
    // CHECK: copy_addr {{.*}} to [[THIS_Y]]
    // CHECK: release

    // -- 'this' was emplaced into indirect return slot
    // CHECK: return
  }

  constructor<U:Intifiable>(xx:U, yy:T) {
    x = xx.intify()
    y = yy
  }
}

class B { constructor(y:Int) {} }
class D : B {
  // CHECK: sil @_TC8lifetime1Dc{{.*}} : $[cc(method), thin] ((x : Int64, y : Int64), D) -> D
  // CHECK: bb0([[X:%[0-9]+]] : $Int64, [[Y:%[0-9]+]] : $Int64, [[THIS:%[0-9]+]] : $D):
  constructor(x:Int, y:Int) {
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
    // CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int64
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_box $D
    // CHECK: store [[THIS]] to [[THISADDR]]

    super.constructor(y)
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: retain [[THIS1]]
    // CHECK: [[THIS1_SUP:%[0-9]+]] = upcast [[THIS1]] : ${{.*}} to $B
    // CHECK: [[SUPER_CTOR:%[0-9]+]] = function_ref @_TC8lifetime1Bc{{.*}}
    // CHECK: [[Y:%[0-9]+]] = load [[YADDR]]
    // CHECK: [[THIS2_SUP:%[0-9]+]] = apply [[SUPER_CTOR]]([[Y]], [[THIS1_SUP]])
    // CHECK: [[THIS2:%[0-9]+]] = downcast unconditional [[THIS2_SUP]] : {{.*}} to $D
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: store [[THIS2]] to [[THISADDR]]
    // CHECK: release [[THIS1]]
  }

  func foo() {}
}

// CHECK: sil @_T8lifetime8downcastFT1bCS_1B_T_
func downcast(b:B) {
  // CHECK: [[BADDR:%[0-9]+]] = alloc_box $B
  (b as! D).foo()
  // CHECK: [[B:%[0-9]+]] = load [[BADDR]]
  // CHECK: retain [[B]]
  // CHECK: [[D:%[0-9]+]] = downcast unconditional [[B]] : {{.*}} to $D
  // CHECK: apply {{.*}}([[D]])
  // CHECK-NOT: release [[B]]
  // CHECK-NOT: release [[D]]
  // CHECK: release [[BADDR]]
  // CHECK: return
}

func int(x:Int) {}
func ref(x:Ref) {}

func tuple() -> (Int, Ref) { return (1, Ref()) }

func tuple_explosion() {
  int(tuple().0)
  // CHECK: [[F:%[0-9]+]] = function_ref @_T8lifetime5tupleFT_TSiCS_3Ref_
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: release [[T1]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release

  ref(tuple().1)
  // CHECK: [[F:%[0-9]+]] = function_ref @_T8lifetime5tupleFT_TSiCS_3Ref_
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release [[T1]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release
}
