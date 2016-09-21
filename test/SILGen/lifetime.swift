// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-as-library -emit-silgen -primary-file %s | %FileCheck %s

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
func local_valtype_branch(_ a: Bool) {
    var a = a
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
    // CHECK: return
}

func reftype_func() -> Ref {}
func reftype_func_with_arg(_ x: Ref) -> Ref {}

// CHECK-LABEL: sil hidden @_TF8lifetime14reftype_returnFT_CS_3Ref
func reftype_return() -> Ref {
    return reftype_func()
    // CHECK: [[RF:%[0-9]+]] = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@convention(thin) () -> @owned Ref
    // CHECK-NOT: release
    // CHECK: [[RET:%[0-9]+]] = apply [[RF]]()
    // CHECK-NOT: release
    // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @_TF8lifetime11reftype_arg
func reftype_arg(_ a: Ref) {
    var a = a
    // CHECK: bb0([[A:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK: [[PA:%[0-9]+]] = project_box [[AADDR]]
    // CHECK: store [[A]] to [[PA]]
    // CHECK: release [[AADDR]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime17reftype_inout_arg
func reftype_inout_arg(_ a: inout Ref) {
    // CHECK: bb0([[A:%[0-9]+]] : $*Ref):
    // -- initialize local box for inout
    // CHECK: [[A_LOCAL:%.*]] = alloc_box $Ref
    // CHECK: [[PB:%.*]] = project_box [[A_LOCAL]]
    // CHECK: copy_addr [[A]] to [initialization] [[PB]]
    // -- write back to inout
    // CHECK: copy_addr [[PB]] to [[A]]
    // CHECK: strong_release [[A_LOCAL]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime26reftype_call_ignore_returnFT_T_
func reftype_call_ignore_return() {
    reftype_func()
    // CHECK: = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@convention(thin) () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK: release [[R]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime27reftype_call_store_to_localFT_T_
func reftype_call_store_to_local() {
    var a = reftype_func()
    // CHECK: [[A:%[0-9]+]] = alloc_box $Ref
    // CHECK-NEXT: [[PB:%.*]] = project_box [[A]]
    // CHECK: = function_ref @_TF8lifetime12reftype_funcFT_CS_3Ref : $@convention(thin) () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK-NOT: retain [[R]]
    // CHECK: store [[R]] to [[PB]]
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
func reftype_call_with_arg(_ a: Ref) {
    var a = a
    // CHECK: bb0([[A1:%[0-9]+]] : $Ref):
    // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK: [[PB:%.*]] = project_box [[AADDR]]
    // CHECK: store [[A1]] to [[PB]]

    reftype_func_with_arg(a)
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @_TF8lifetime21reftype_func_with_arg
    // CHECK: [[A2:%[0-9]+]] = load [[PB]]
    // CHECK: retain [[A2]]
    // CHECK: = apply [[RFWA]]([[A2]])

    // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8lifetime16reftype_reassign
func reftype_reassign(_ a: inout Ref, b: Ref) {
    var b = b
    // CHECK: bb0([[AADDR:%[0-9]+]] : $*Ref, [[B1:%[0-9]+]] : $Ref):
    // CHECK: [[A_LOCAL:%[0-9]+]] = alloc_box $Ref
    // CHECK: [[PBA:%.*]] = project_box [[A_LOCAL]]
    // CHECK: copy_addr [[AADDR]] to [initialization] [[PBA]]
    // CHECK: [[BADDR:%[0-9]+]] = alloc_box $Ref
    // CHECK: [[PBB:%.*]] = project_box [[BADDR]]
    a = b
    // CHECK: copy_addr [[PBB]] to [[PBA]]
    // CHECK: release

    // CHECK: return
}

func tuple_with_ref_elements() -> (Val, (Ref, Val), Ref) {}

// CHECK-LABEL: sil hidden @_TF8lifetime28tuple_with_ref_ignore_returnFT_T_
func tuple_with_ref_ignore_return() {
  tuple_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF8lifetime23tuple_with_ref_elementsFT_TVS_3ValTCS_3RefS0__S1__
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[T0:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 0
  // CHECK: [[T1_0:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: [[T1_1:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 2
  // CHECK: [[T2:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 3
  // CHECK: release [[T2]]
  // CHECK: release [[T1_0]]
  // CHECK: return
}

struct Aleph {
  var a:Ref
  var b:Val

  // -- loadable value constructor:
  // CHECK-LABEL: sil hidden @_TFV8lifetime5AlephC{{.*}} : $@convention(method) (@owned Ref, Val, @thin Aleph.Type) -> @owned Aleph
  // CHECK: bb0([[A:%.*]] : $Ref, [[B:%.*]] : $Val, {{%.*}} : $@thin Aleph.Type):
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
  // CHECK-LABEL: sil hidden @_TFV8lifetime6DalethC{{.*}} : $@convention(method) (@owned Aleph, @owned Beth, @in Unloadable, @thin Daleth.Type) -> @out Daleth {
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
  // CHECK-LABEL: sil hidden @_TFC8lifetime2Hec{{.*}} : $@convention(method) (@owned He) -> @owned He {
  // CHECK: bb0([[THIS:%.*]] : $He):
  // CHECK-NEXT: debug_value
  // CHECK-NEXT: mark_uninitialized
  // CHECK-NEXT: return
  // CHECK: }

  // -- default allocator:
  // CHECK-LABEL: sil hidden @_TFC8lifetime2HeC{{.*}} : $@convention(method) (@thick He.Type) -> @owned He {
  // CHECK: bb0({{%.*}} : $@thick He.Type):
  // CHECK-NEXT:   [[THIS:%.*]] = alloc_ref $He
  // CHECK-NEXT:   // function_ref lifetime.He.init
  // CHECK-NEXT:   [[INIT:%.*]] = function_ref @_TFC8lifetime2Hec{{.*}} : $@convention(method) (@owned He) -> @owned He
  // CHECK-NEXT:   [[THIS1:%.*]] = apply [[INIT]]([[THIS]])
  // CHECK-NEXT:   return [[THIS1]]
  // CHECK-NEXT: }
  init() { }
}

struct Waw {
  var a:(Ref, Val)
  var b:Val

  // -- loadable value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @_TFV8lifetime3WawC{{.*}} : $@convention(method) (@owned Ref, Val, Val, @thin Waw.Type) -> @owned Waw 
  // CHECK: bb0([[A0:%.*]] : $Ref, [[A1:%.*]] : $Val, [[B:%.*]] : $Val, {{%.*}} : $@thin Waw.Type):
  // CHECK-NEXT:   [[A:%.*]] = tuple ([[A0]] : {{.*}}, [[A1]] : {{.*}})
  // CHECK-NEXT:   [[RET:%.*]] = struct $Waw ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]
}

struct Zayin {
  var a:(Unloadable, Val)
  var b:Unloadable

  // -- address-only value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @_TFV8lifetime5ZayinC{{.*}} : $@convention(method) (@in Unloadable, Val, @in Unloadable, @thin Zayin.Type) -> @out Zayin
  // CHECK: bb0([[THIS:%.*]] : $*Zayin, [[A0:%.*]] : $*Unloadable, [[A1:%.*]] : $Val, [[B:%.*]] : $*Unloadable, {{%.*}} : $@thin Zayin.Type):
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
  // CHECK: [[METHOD:%[0-9]+]] = function_ref @_TFV8lifetime4Beth5gimel{{.*}}
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF8lifetime32fragile_struct_with_ref_elementsFT_VS_4Beth
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: apply [[METHOD]]([[STRUCT]])
}

class RefWithProp {
  var int_prop: Int { get {} set {} }
  var aleph_prop: Aleph { get {} set {} }
}

// CHECK-LABEL: sil hidden @_TF8lifetime23logical_lvalue_lifetimeFTCS_11RefWithPropSiVS_3Val_T_ : $@convention(thin) (@owned RefWithProp, Int, Val) -> () {
func logical_lvalue_lifetime(_ r: RefWithProp, _ i: Int, _ v: Val) {
  var r = r
  var i = i
  var v = v
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box $RefWithProp
  // CHECK: [[PR:%[0-9]+]] = project_box [[RADDR]]
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[PI:%[0-9]+]] = project_box [[IADDR]]
  // CHECK: store %1 to [[PI]]
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  // CHECK: [[PV:%[0-9]+]] = project_box [[VADDR]]

  // -- Reference types need to be retained as property method args.
  r.int_prop = i
  // CHECK: [[R1:%[0-9]+]] = load [[PR]]
  // CHECK: strong_retain [[R1]]
  // CHECK: [[SETTER_METHOD:%[0-9]+]] = class_method {{.*}} : $RefWithProp, #RefWithProp.int_prop!setter.1 : (RefWithProp) -> (Int) -> () , $@convention(method) (Int, @guaranteed RefWithProp) -> ()
  // CHECK: apply [[SETTER_METHOD]]({{.*}}, [[R1]])
  // CHECK: strong_release [[R1]]

  r.aleph_prop.b = v
  // CHECK: [[R2:%[0-9]+]] = load [[PR]]
  // CHECK: strong_retain [[R2]]
  // CHECK: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: [[ALEPH_PROP_TEMP:%[0-9]+]] = alloc_stack $Aleph
  // CHECK: [[T0:%.*]] = address_to_pointer [[ALEPH_PROP_TEMP]]
  // CHECK: [[MATERIALIZE_METHOD:%[0-9]+]] = class_method {{.*}} : $RefWithProp, #RefWithProp.aleph_prop!materializeForSet.1 :
  // CHECK: [[MATERIALIZE:%.*]] = apply [[MATERIALIZE_METHOD]]([[T0]], [[STORAGE]], [[R2]])
  // CHECK: [[PTR:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 0
  // CHECK: [[OPTCALLBACK:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 1
  // CHECK: [[ADDR:%.*]] = pointer_to_address [[PTR]]
  // CHECK: [[MARKED_ADDR:%.*]] = mark_dependence [[ADDR]] : $*Aleph on [[R2]]
  // CHECK: {{.*}}([[CALLBACK_ADDR:%.*]] : 
  // CHECK: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout RefWithProp, @thick RefWithProp.Type) -> ()
  // CHECK: [[TEMP:%.*]] = alloc_stack $RefWithProp
  // CHECK: store [[R2]] to [[TEMP]]
  // CHECK: apply [[CALLBACK]]({{.*}}, [[STORAGE]], [[TEMP]], {{%.*}})
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
  // CHECK-LABEL: sil hidden @_TFC8lifetime3Fooc{{.*}} :
    // CHECK: bb0([[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THIS:%[0-9]+]] = mark_uninitialized

    // initialization for y
    // CHECK: [[Y_INIT:%[0-9]+]] = function_ref @_TIvC8lifetime3Foo1yTSiCS_3Ref_i : $@convention(thin) <τ_0_0> () -> (Int, @owned Ref)
    // CHECK: [[Y_VALUE:%[0-9]+]] = apply [[Y_INIT]]<T>()

    x = bar()
    // CHECK: function_ref @_TF8lifetime3barFT_Si : $@convention(thin) () -> Int
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // CHECK: assign {{.*}} to [[THIS_X]]

    z = Foo<T>.makeT()
    // CHECK: [[FOOMETA:%[0-9]+]] = metatype $@thick Foo<T>.Type
    // CHECK: [[MAKET:%[0-9]+]] = class_method [[FOOMETA]] : {{.*}}, #Foo.makeT!1
    // CHECK: ref_element_addr

    // -- cleanup this lvalue and return this
    // CHECK: return [[THIS]]

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooC{{.*}} :
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TFC8lifetime3Fooc{{.*}}
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_METHOD]]<{{.*}}>([[THIS]])
    // CHECK: return [[INIT_THIS]]

  }

  init(chi:Int) {
    var chi = chi
    z = Foo<T>.makeT()

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FoocfT3chiSi_GS0_x_
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THIS:%[0-9]+]] = mark_uninitialized
    // CHECK: [[CHIADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[PCHI:%[0-9]+]] = project_box [[CHIADDR]]
    // CHECK: store [[CHI]] to [[PCHI]]

    // CHECK: ref_element_addr {{.*}}, #Foo.z

    x = chi
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // CHECK: copy_addr [[PCHI]] to [[THIS_X]]

    // -- cleanup chi
    // CHECK: release [[CHIADDR]]
    // CHECK: return [[THIS]]

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooC{{.*}} :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @_TFC8lifetime3Fooc{{.*}}
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
  // CHECK-LABEL: sil hidden @_TFC8lifetime3FooD : $@convention(method) <T> (@owned Foo<T>) -> ()
  // CHECK: bb0([[SELF:%[0-9]+]] : $Foo<T>):
  // CHECK:   [[DESTROYING_REF:%[0-9]+]] = function_ref @_TFC8lifetime3Food : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   [[RESULT_SELF:%[0-9]+]] = apply [[DESTROYING_REF]]<T>([[SELF]]) : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   [[SELF:%[0-9]+]] = unchecked_ref_cast [[RESULT_SELF]] : $Builtin.NativeObject to $Foo<T>
  // CHECK-NEXT:   dealloc_ref [[SELF]] : $Foo<T>
  // CHECK-NEXT:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT:   return [[RESULT]] : $()
  // CHECK-LABEL: sil hidden @_TFC8lifetime3Food : $@convention(method) <T> (@guaranteed Foo<T>) -> @owned Builtin.NativeObject

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

  // CHECK-LABEL: sil hidden @_TFC8lifetime30ImplicitDtorDerivedFromGenericc{{.*}}
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
    // CHECK: [[PB:%.*]] = project_box [[THISADDRBOX]]
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [rootself] [[PB]]

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
    // CHECK: [[PB:%.*]] = project_box [[THISADDRBOX]]
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [rootself] [[PB]]

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
  // CHECK-LABEL: sil hidden @_TFC8lifetime1DcfT1xSi1ySi_S0_
  // CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $D):
  init(x: Int, y: Int) {
    var x = x
    var y = y
    // CHECK: [[THISADDR1:%[0-9]+]] = alloc_box $D
    // CHECK: [[PTHIS:%[0-9]+]] = project_box [[THISADDR1]]
    // CHECK: [[THISADDR:%[0-9]+]] = mark_uninitialized [derivedself] [[PTHIS]]
    // CHECK: store [[THIS]] to [[THISADDR]]
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[PX:%[0-9]+]] = project_box [[XADDR]]
    // CHECK: store [[X]] to [[PX]]
    // CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[PY:%[0-9]+]] = project_box [[YADDR]]
    // CHECK: store [[Y]] to [[PY]]

    super.init(y: y)
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: [[THIS1_SUP:%[0-9]+]] = upcast [[THIS1]] : ${{.*}} to $B
    // CHECK: [[SUPER_CTOR:%[0-9]+]] = function_ref @_TFC8lifetime1BcfT1ySi_S0_ : $@convention(method) (Int, @owned B) -> @owned B
    // CHECK: [[Y:%[0-9]+]] = load [[PY]]
    // CHECK: [[THIS2_SUP:%[0-9]+]] = apply [[SUPER_CTOR]]([[Y]], [[THIS1_SUP]])
    // CHECK: [[THIS2:%[0-9]+]] = unchecked_ref_cast [[THIS2_SUP]] : $B to $D
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: release 
  }

  func foo() {}
}

// CHECK-LABEL: sil hidden @_TF8lifetime8downcast
func downcast(_ b: B) {
  var b = b
  // CHECK: [[BADDR:%[0-9]+]] = alloc_box $B
  // CHECK: [[PB:%[0-9]+]] = project_box [[BADDR]]
  (b as! D).foo()
  // CHECK: [[B:%[0-9]+]] = load [[PB]]
  // CHECK: retain [[B]]
  // CHECK: [[D:%[0-9]+]] = unconditional_checked_cast [[B]] : {{.*}} to $D
  // CHECK: apply {{.*}}([[D]])
  // CHECK-NOT: release [[B]]
  // CHECK: release [[D]]
  // CHECK: release [[BADDR]]
  // CHECK: return
}

func int(_ x: Int) {}
func ref(_ x: Ref) {}

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

class C {
  var v = ""
  // CHECK-LABEL: sil hidden @_TFC8lifetime1C18ignored_assignment{{.*}}
  func ignored_assignment() {
    // CHECK: [[STRING:%.*]] = alloc_stack $String
    // CHECK: [[UNINIT:%.*]] = mark_uninitialized [var] [[STRING]]
    // CHECK: assign {{%.*}} to [[UNINIT]]
    // CHECK: destroy_addr [[UNINIT]]
    _ = self.v
  }
}
