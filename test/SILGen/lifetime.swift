// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s

struct Buh<T> {
  var x: Int {
    get {}
    set {}
  }
}

class Ref {
  init() { }
}
struct Val {
}

// CHECK-LABEL: sil hidden @_TF8lifetime13local_valtypeFT_T_
func local_valtype() {
    var b: Val
    // CHECK: [[B:%[0-9]+]] = alloc_box $Val
    // CHECK: release [[B]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime20local_valtype_branch
func local_valtype_branch(var a: Bool) {
    // CHECK: [[A:%[0-9]+]] = alloc_box $Bool

    if a { return }
    // CHECK: cond_br
    // CHECK: {{bb.*:}}
    // CHECK: br [[EPILOG:bb[0-9]+]]

    var x:Int
    // CHECK: [[X:%[0-9]+]] = alloc_box $Int

    if a { return }
    // CHECK: cond_br
    // CHECK: {{bb.*:}}
    // CHECK: release [[X]]
    // CHECK: br [[EPILOG]]

    while a {
    // CHECK: cond_br
        if a { break }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK-NOT: release [[X]]
        // CHECK: br

        if a { return }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: release [[X]]
        // CHECK: br [[EPILOG]]

        var y:Int
        // CHECK: [[Y:%[0-9]+]] = alloc_box $Int

        if a { break }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK-NOT: release [[X]]
        // CHECK-NOT: release [[A]]
        // CHECK: br

        if a { return }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: release [[Y]]
        // CHECK: release [[X]]
        // CHECK: br [[EPILOG]]

        if true {
            var z:Int
            // CHECK: [[Z:%[0-9]+]] = alloc_box $Int

            if a { break }
            // CHECK: cond_br
            // CHECK: {{bb.*:}}
            // CHECK: release [[Z]]
            // CHECK: release [[Y]]
            // CHECK-NOT: release [[X]]
            // CHECK-NOT: release [[A]]
            // CHECK: br

            if a { return }
            // CHECK: cond_br
            // CHECK: {{bb.*:}}
            // CHECK: release [[Z]]
            // CHECK: release [[Y]]
            // CHECK: release [[X]]
            // CHECK: br [[EPILOG]]

            // CHECK: release [[Z]]
        }
        if a { break }
        // CHECK: cond_br
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
func reftype_func_with_arg(x: Ref) -> Ref {}

// CHECK-LABEL: sil hidden @_TF8lifetime14reftype_returnFT_CS_3Ref
func reftype_return() -> Ref {
    return reftype_func()
    // CHECK: [[RF:%[0-9]+]] = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@thin () -> @owned Ref
    // CHECK-NOT: release
    // CHECK: [[RET:%[0-9]+]] = apply [[RF]]()
    // CHECK-NOT: release
    // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @_TF8lifetime11reftype_arg
func reftype_arg(var a: Ref) {
    // CHECK: bb0([[A:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK-NOT: retain [[A]]
    // CHECK: store [[A]] to [[AADDR]]
    // CHECK: release [[AADDR]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime17reftype_inout_arg
func reftype_inout_arg(inout a: Ref) {
    // CHECK: bb0([[A:%[0-9]+]] : $*Ref):
    // -- initialize local box for inout
    // CHECK: [[A_LOCAL:%.*]] = alloc_box $Ref
    // CHECK: copy_addr [[A]] to [initialization] [[A_LOCAL]]
    // -- write back to inout
    // CHECK: copy_addr [[A_LOCAL]]#1 to [[A]]
    // CHECK: strong_release [[A_LOCAL]]#0
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime26reftype_call_ignore_returnFT_T_
func reftype_call_ignore_return() {
    reftype_func()
    // CHECK: = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@thin () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK: release [[R]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime27reftype_call_store_to_localFT_T_
func reftype_call_store_to_local() {
    var a = reftype_func()
    // CHECK: [[A:%[0-9]+]] = alloc_box $Ref
    // CHECK: = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@thin () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK-NOT: retain [[R]]
    // CHECK: store [[R]] to [[A]]
    // CHECK-NOT: release [[R]]
    // CHECK: release [[A]]
    // CHECK-NOT: release [[R]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime16reftype_call_argFT_T_
func reftype_call_arg() {
    reftype_func_with_arg(reftype_func())
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @_TF8lifetime21reftype_func_with_arg
    // CHECK: [[RF:%[0-9]+]] = function_ref @_TF8lifetime12reftype_func
    // CHECK: [[R1:%[0-9]+]] = apply [[RF]]
    // CHECK: [[R2:%[0-9]+]] = apply [[RFWA]]([[R1]])
    // CHECK: release [[R2]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime21reftype_call_with_arg
func reftype_call_with_arg(var a: Ref) {
    // CHECK: bb0([[A1:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK: store [[A1]] to [[AADDR]]

    reftype_func_with_arg(a)
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @_TF8lifetime21reftype_func_with_arg
    // CHECK: [[A2:%[0-9]+]] = load [[AADDR]]
    // CHECK: retain [[A2]]
    // CHECK: = apply [[RFWA]]([[A2]])

    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime16reftype_reassign
func reftype_reassign(inout a: Ref, var b: Ref) {
    // CHECK: bb0([[AADDR:%[0-9]+]] : $*Ref, [[B1:%[0-9]+]] : $Ref):
    // CHECK: [[A_LOCAL:%[0-9]+]] = alloc_box $Ref
    // CHECK: copy_addr [[AADDR]] to [initialization] [[A_LOCAL]]
    // CHECK: [[BADDR:%[0-9]+]] = alloc_box $Ref
    a = b
    // CHECK: copy_addr [[BADDR]]#1 to [[A_LOCAL]]
    // CHECK: release

    // CHECK: return
}

func tuple_with_ref_elements() -> (Val, (Ref, Val), Ref) {}

// CHECK-LABEL: sil hidden @_TF8lifetime28tuple_with_ref_ignore_returnFT_T_
func tuple_with_ref_ignore_return() {
  tuple_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF8lifetime23tuple_with_ref_elementsFT_TVS_3ValTCS_3RefS0__S1__
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
  // CHECK-LABEL: sil hidden @_TFV8lifetime5AlephCfMS0_FT1aCS_3Ref1bVS_3Val_S0_ : $@thin (@owned Ref, Val, @thin Aleph.Type) -> @owned Aleph
  // CHECK-NEXT: bb0([[A:%.*]] : $Ref, [[B:%.*]] : $Val, {{%.*}} : $@thin Aleph.Type):
  // CHECK-NEXT:   [[RET:%.*]] = struct $Aleph ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]
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
  // CHECK-LABEL: sil hidden @_TFV8lifetime6DalethCfMS0_FT1aVS_5Aleph1bVS_4Beth1cPS_10Unloadable__S0_ : $@thin (@out Daleth, @owned Aleph, @owned Beth, @in Unloadable, @thin Daleth.Type) -> () {
  // CHECK: bb0([[THIS:%.*]] : $*Daleth, [[A:%.*]] : $Aleph, [[B:%.*]] : $Beth, [[C:%.*]] : $*Unloadable, {{%.*}} : $@thin Daleth.Type):
  // CHECK-NEXT:   [[A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.a
  // CHECK-NEXT:   store [[A]] to [[A_ADDR]]
  // CHECK-NEXT:   [[B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.b
  // CHECK-NEXT:   store [[B]] to [[B_ADDR]]
  // CHECK-NEXT:   [[C_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.c
  // CHECK-NEXT:   copy_addr [take] [[C]] to [initialization] [[C_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

class He {
  // -- default initializer:
  // CHECK-LABEL: sil hidden @_TFC8lifetime2HecfMS0_FT_S0_ : $@cc(method) @thin (@owned He) -> @owned He {
  // CHECK: bb0([[THIS:%.*]] : $He):
  // CHECK-NEXT: debug_value
  // CHECK-NEXT: mark_uninitialized
  // CHECK-NEXT: return
  // CHECK: }

  // -- default allocator:
  // CHECK-LABEL: sil hidden @_TFC8lifetime2HeCfMS0_FT_S0_ : $@thin (@thick He.Type) -> @owned He {
  // CHECK-NEXT: bb0({{%.*}} : $@thick He.Type):
  // CHECK-NEXT:   [[THIS:%.*]] = alloc_ref $He
  // CHECK-NEXT:   // function_ref lifetime.He.init
  // CHECK-NEXT:   [[INIT:%.*]] = function_ref @_TFC8lifetime2HecfMS0_FT_S0_ : $@cc(method) @thin (@owned He) -> @owned He
  // CHECK-NEXT:   [[THIS1:%.*]] = apply [[INIT]]([[THIS]])
  // CHECK-NEXT:   return [[THIS1]]
  // CHECK-NEXT: }
  init() { }
}

struct Waw {
  var a:(Ref, Val)
  var b:Val

  // -- loadable value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @_TFV8lifetime3WawCfMS0_FT1aTCS_3RefVS_3Val_1bS2__S0_ : $@thin (@owned Ref, Val, Val, @thin Waw.Type) -> @owned Waw 
  // CHECK-NEXT: bb0([[A0:%.*]] : $Ref, [[A1:%.*]] : $Val, [[B:%.*]] : $Val, {{%.*}} : $@thin Waw.Type):
  // CHECK-NEXT:   [[A:%.*]] = tuple ([[A0]] : {{.*}}, [[A1]] : {{.*}})
  // CHECK-NEXT:   [[RET:%.*]] = struct $Waw ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]
}

struct Zayin {
  var a:(Unloadable, Val)
  var b:Unloadable

  // -- address-only value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @_TFV8lifetime5ZayinCfMS0_FT1aTPS_10Unloadable_VS_3Val_1bPS1___S0_ : $@thin (@out Zayin, @in Unloadable, Val, @in Unloadable, @thin Zayin.Type) -> ()
  // CHECK-NEXT: bb0([[THIS:%.*]] : $*Zayin, [[A0:%.*]] : $*Unloadable, [[A1:%.*]] : $Val, [[B:%.*]] : $*Unloadable, {{%.*}} : $@thin Zayin.Type):
  // CHECK-NEXT:   [[THIS_A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #Zayin.a
  // CHECK-NEXT:   [[THIS_A0_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 0
  // CHECK-NEXT:   [[THIS_A1_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 1
  // CHECK-NEXT:   copy_addr [take] [[A0]] to [initialization] [[THIS_A0_ADDR]]
  // CHECK-NEXT:   store [[A1]] to [[THIS_A1_ADDR]]
  // CHECK-NEXT:   [[THIS_B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #Zayin.b
  // CHECK-NEXT:   copy_addr [take] [[B]] to [initialization] [[THIS_B_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

func fragile_struct_with_ref_elements() -> Beth {}

// CHECK-LABEL: sil hidden @_TF8lifetime29struct_with_ref_ignore_returnFT_T_
func struct_with_ref_ignore_return() {
  fragile_struct_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF8lifetime32fragile_struct_with_ref_elementsFT_VS_4Beth
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: release_value [[STRUCT]] : $Beth
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime28struct_with_ref_materializedFT_T_
func struct_with_ref_materialized() {
  fragile_struct_with_ref_elements().gimel()
  // CHECK: [[METHOD:%[0-9]+]] = function_ref @_TFV8lifetime4Beth5gimelfS0_FT_T_
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF8lifetime32fragile_struct_with_ref_elementsFT_VS_4Beth
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: apply [[METHOD]]([[STRUCT]])
}

class RefWithProp {
  var int_prop: Int { get {} set {} }
  var aleph_prop: Aleph { get {} set {} }
}

// CHECK-LABEL: sil hidden @_TF8lifetime23logical_lvalue_lifetime
func logical_lvalue_lifetime(var r: RefWithProp, var i: Int, var v: Val) {
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box $RefWithProp
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val

  // -- Reference types need to be retained as property method args.
  r.int_prop = i
  // CHECK: [[R1:%[0-9]+]] = load [[RADDR]]
  // CHECK: retain [[R1]]
  // CHECK: [[SETTER_METHOD:%[0-9]+]] = class_method {{.*}} : $RefWithProp, #RefWithProp.int_prop!setter.1 : RefWithProp -> (Int) -> ()
  // CHECK: apply [[SETTER_METHOD]]({{.*}}, [[R1]])

  r.aleph_prop.b = v
  // CHECK: [[R2:%[0-9]+]] = load [[RADDR]]
  // CHECK: retain [[R2]]
  // CHECK: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: [[ALEPH_PROP_TEMP:%[0-9]+]] = alloc_stack $Aleph
  // CHECK: retain [[R2]]
  // CHECK: [[T0:%.*]] = address_to_pointer [[ALEPH_PROP_TEMP]]#1
  // CHECK: [[MATERIALIZE_METHOD:%[0-9]+]] = class_method {{.*}} : $RefWithProp, #RefWithProp.aleph_prop!materializeForSet.1 :
  // CHECK: [[MATERIALIZE:%.*]] = apply [[MATERIALIZE_METHOD]]([[T0]], [[STORAGE]]#1, [[R2]])
  // CHECK: [[PTR:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 0
  // CHECK: [[ADDR:%.*]] = pointer_to_address [[PTR]]
  // CHECK: [[OPTCALLBACK:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 1
  // CHECK: [[MARKED_ADDR:%.*]] = mark_dependence [[ADDR]] : $*Aleph on [[R2]]
  // CHECK: {{.*}}([[CALLBACK:%.*]] : 
  // CHECK: [[TEMP:%.*]] = alloc_stack $RefWithProp
  // CHECK: store [[R2]] to [[TEMP]]#1
  // CHECK: apply [[CALLBACK]]({{.*}}, [[STORAGE]]#1, [[TEMP]]#1, {{%.*}})
}

func bar() -> Int {}

class Foo<T> {
  var x : Int
  var y = (Int(), Ref())
  var z : T
  var w = Ref()

  class func makeT() -> T {}

  // Class initializer
  init() {
  // -- initializing entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FoocU__fMGS0_Q__FT_GS0_Q__ :
    // CHECK: bb0([[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THIS:%[0-9]+]] = mark_uninitialized

    // initialization for y
    // CHECK: [[INTCTOR:%[0-9]+]] = function_ref @_TFSiCfMSiFT_Si : $@thin (@thin Int.Type) -> Int
    // CHECK: [[INTMETA:%[0-9]+]] = metatype $@thin Int.Type
    // CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCTOR]]([[INTMETA]])

    x = bar()
    // CHECK: function_ref @_TF8lifetime3barFT_Si : $@thin () -> Int
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // CHECK: assign {{.*}} to [[THIS_X]]

    z = Foo<T>.makeT()
    // CHECK: [[FOOMETA:%[0-9]+]] = metatype $@thick Foo<T>.Type
    // CHECK: [[MAKET:%[0-9]+]] = class_method [[FOOMETA]] : {{.*}}, #Foo.makeT!1
    // CHECK: ref_element_addr

    // -- cleanup this lvalue and return this
    // CHECK: return [[THIS]]

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooCU__fMGS0_Q__FT_GS0_Q__ :
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TFC8lifetime3FoocU__fMGS0_Q__FT_GS0_Q__
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_METHOD]]<{{.*}}>([[THIS]])
    // CHECK: return [[INIT_THIS]]

  }

  init(var chi:Int) {
    z = Foo<T>.makeT()

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FoocU__fMGS0_Q__FT3chiSi_GS0_Q__ :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK: [[CHIADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: store [[CHI]] to [[CHIADDR]]
    // CHECK: [[THIS:%[0-9]+]] = mark_uninitialized

    // CHECK: ref_element_addr {{.*}}, #Foo.z

    x = chi
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // CHECK: copy_addr [[CHIADDR]]#1 to [[THIS_X]]

    // -- cleanup chi
    // CHECK: release [[CHIADDR]]
    // CHECK: return [[THIS]]

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooCU__fMGS0_Q__FT3chiSi_GS0_Q__ :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TFC8lifetime3FoocU__fMGS0_Q__FT3chiSi_GS0_Q__
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_METHOD]]<{{.*}}>([[CHI]], [[THIS]])
    // CHECK: return [[INIT_THIS]]
  }

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3Fooc{{.*}} :

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooC{{.*}} :
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TFC8lifetime3Fooc{{.*}}
  init<U:Intifiable>(chi:U) {
    z = Foo<T>.makeT()

    x = chi.intify()
  }

  // Deallocating destructor for Foo.
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooD : $@cc(method) @thin <T> (@owned Foo<T>) -> ()
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $Foo<T>):
  // CHECK:   [[DESTROYING_REF:%[0-9]+]] = function_ref @_TFC8lifetime3Food : $@cc(method) @thin <τ_0_0> (@owned Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   [[RESULT_SELF:%[0-9]+]] = apply [[DESTROYING_REF]]<T>([[SELF]]) : $@cc(method) @thin <τ_0_0> (@owned Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   [[SELF:%[0-9]+]] = unchecked_ref_cast [[RESULT_SELF]] : $Builtin.NativeObject to $Foo<T>
  // CHECK-NEXT:   dealloc_ref [[SELF]] : $Foo<T>
  // CHECK-NEXT:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT:   return [[RESULT]] : $()
  // CHECK-LABEL: sil hidden @_TFC8lifetime3Food : $@cc(method) @thin <T> (@owned Foo<T>) -> @owned Builtin.NativeObject

  deinit {
    // CHECK: bb0([[THIS:%[0-9]+]] : $Foo<T>):
    bar()
    // CHECK: function_ref @_TF8lifetime3barFT_Si
    // CHECK: apply

    // CHECK: [[PTR:%.*]] = unchecked_ref_cast [[THIS]] : ${{.*}} to $Builtin.NativeObject

    // -- don't need to release x because it's trivial
    // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // -- release y
    // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.y
    // CHECK: destroy_addr [[YADDR]]
    // -- release z
    // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.z
    // CHECK: destroy_addr [[ZADDR]]
    // -- release w
    // CHECK: [[WADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.w
    // CHECK: destroy_addr [[WADDR]]
    // -- return back this
    // CHECK: return [[PTR]]
  }

}

class ImplicitDtor {
  var x:Int
  var y:(Int, Ref)
  var w:Ref
  init() { }

  // CHECK-LABEL: sil hidden @_TFC8lifetime12ImplicitDtord
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtor):
  // -- don't need to release x because it's trivial
  // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtor.x
  // -- release y
  // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtor.y
  // CHECK: destroy_addr [[YADDR]]
  // -- release w
  // CHECK: [[WADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtor.w
  // CHECK: destroy_addr [[WADDR]]
  // CHECK: return
}

class ImplicitDtorDerived<T> : ImplicitDtor {
  var z:T

  init(z : T) { 
    super.init() 
    self.z = z
  }

  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerived<T>):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtor
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @_TFC8lifetime12ImplicitDtord
  // CHECK: apply [[BASE_DTOR]]([[BASE]])
  // -- release z
  // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtorDerived.z
  // CHECK: destroy_addr [[ZADDR]]
}

class ImplicitDtorDerivedFromGeneric<T> : ImplicitDtorDerived<Int> {
  init() { super.init(z: 5) }

  // CHECK-LABEL: sil hidden @_TFC8lifetime30ImplicitDtorDerivedFromGenericcU__fMGS0_Q__FT_GS0_Q__
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerivedFromGeneric<T>):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtorDerived<Int>
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @_TFC8lifetime19ImplicitDtorDerivedd
  // CHECK: [[PTR:%.*]] = apply [[BASE_DTOR]]<Int>([[BASE]])
  // CHECK: return [[PTR]]
}

protocol Intifiable {
  func intify() -> Int
}

struct Bar {
  var x:Int

  // Loadable struct initializer
  // CHECK-LABEL: sil hidden @_TFV8lifetime3BarC{{.*}}
  init() {
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $@thin Bar.Type):
    // CHECK: [[THISADDRBOX:%[0-9]+]] = alloc_box $Bar
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [rootself] [[THISADDRBOX]]

    x = bar()
    // CHECK: [[THIS_X:%[0-9]+]] = struct_element_addr [[THISADDR]] : $*Bar, #Bar.x
    // CHECK: assign {{.*}} to [[THIS_X]]

    // -- load and return this
    // CHECK: [[THISVAL:%[0-9]+]] = load [[THISADDR]]
    // CHECK: release [[THISADDRBOX]]
    // CHECK: return [[THISVAL]]
  }

  init<T:Intifiable>(xx:T) {
    x = xx.intify()
  }
}

struct Bas<T> {
  var x:Int
  var y:T

  // Address-only struct initializer
  // CHECK-LABEL: sil hidden @_TFV8lifetime3BasC{{.*}}
  init(yy:T) {
    // CHECK: bb0([[THISADDRPTR:%[0-9]+]] : $*Bas<T>, [[YYADDR:%[0-9]+]] : $*T, [[META:%[0-9]+]] : $@thin Bas<T>.Type):
    // CHECK: alloc_box
    // CHECK: [[THISADDRBOX:%[0-9]+]] = alloc_box $Bas
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [rootself] [[THISADDRBOX]]

    x = bar()
    // CHECK: [[THIS_X:%[0-9]+]] = struct_element_addr [[THISADDR]] : $*Bas<T>, #Bas.x
    // CHECK: assign {{.*}} to [[THIS_X]]

    y = yy
    // CHECK: [[THIS_Y:%[0-9]+]] = struct_element_addr [[THISADDR]] : $*Bas<T>, #Bas.y
    // CHECK: copy_addr {{.*}} to [[THIS_Y]]
    // CHECK: release

    // -- 'self' was emplaced into indirect return slot
    // CHECK: return
  }

  init<U:Intifiable>(xx:U, yy:T) {
    x = xx.intify()
    y = yy
  }
}

class B { init(y:Int) {} }
class D : B {
  // CHECK-LABEL: sil hidden @_TFC8lifetime1Dc{{.*}} : $@cc(method) @thin (Int, Int, @owned D) -> @owned D
  // CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $D):
  init(var x:Int, var y:Int) {
    // CHECK: [[THISADDR1:%[0-9]+]] = alloc_box $D
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [derivedself] [[THISADDR1]]
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: store [[THIS]] to [[THISADDR]]

    super.init(y: y)
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: [[THIS1_SUP:%[0-9]+]] = upcast [[THIS1]] : ${{.*}} to $B
    // CHECK: [[SUPER_CTOR:%[0-9]+]] = function_ref @_TFC8lifetime1Bc{{.*}}
    // CHECK: [[Y:%[0-9]+]] = load [[YADDR]]
    // CHECK: [[THIS2_SUP:%[0-9]+]] = apply [[SUPER_CTOR]]([[Y]], [[THIS1_SUP]])
    // CHECK: [[THIS2:%[0-9]+]] = unchecked_ref_cast [[THIS2_SUP]] : $B to $D
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: release 
  }

  func foo() {}
}

// CHECK-LABEL: sil hidden @_TF8lifetime8downcast
func downcast(var b: B) {
  // CHECK: [[BADDR:%[0-9]+]] = alloc_box $B
  (b as! D).foo()
  // CHECK: [[B:%[0-9]+]] = load [[BADDR]]
  // CHECK: retain [[B]]
  // CHECK: [[D:%[0-9]+]] = unconditional_checked_cast [[B]] : {{.*}} to $D
  // CHECK: apply {{.*}}([[D]])
  // CHECK-NOT: release [[B]]
  // CHECK-NOT: release [[D]]
  // CHECK: release [[BADDR]]
  // CHECK: return
}

func int(x: Int) {}
func ref(x: Ref) {}

func tuple() -> (Int, Ref) { return (1, Ref()) }

func tuple_explosion() {
  int(tuple().0)
  // CHECK: [[F:%[0-9]+]] = function_ref @_TF8lifetime5tupleFT_TSiCS_3Ref_
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: release [[T1]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release

  ref(tuple().1)
  // CHECK: [[F:%[0-9]+]] = function_ref @_TF8lifetime5tupleFT_TSiCS_3Ref_
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release [[T1]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: release
}
