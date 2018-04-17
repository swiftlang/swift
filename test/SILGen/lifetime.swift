
// RUN: %target-swift-frontend -module-name lifetime -Xllvm -sil-full-demangle -parse-as-library -emit-silgen -primary-file %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @$S8lifetime13local_valtypeyyF
func local_valtype() {
    var b: Val
    // CHECK: [[B:%[0-9]+]] = alloc_box ${ var Val }
    // CHECK: [[MARKED_B:%.*]] = mark_uninitialized [var] [[B]]
    // CHECK: destroy_value [[MARKED_B]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @$S8lifetime20local_valtype_branch{{[_0-9a-zA-Z]*}}F
func local_valtype_branch(_ a: Bool) {
    var a = a
    // CHECK: [[A:%[0-9]+]] = alloc_box ${ var Bool }

    if a { return }
    // CHECK: cond_br
    // CHECK: {{bb.*:}}
    // CHECK: br [[EPILOG:bb[0-9]+]]

    var x:Int
    // CHECK: [[X:%[0-9]+]] = alloc_box ${ var Int }
    // CHECK: [[MARKED_X:%.*]] = mark_uninitialized [var] [[X]]

    if a { return }
    // CHECK: cond_br
    // CHECK: {{bb.*:}}
    // CHECK: destroy_value [[MARKED_X]]
    // CHECK: br [[EPILOG]]

    while a {
    // CHECK: cond_br
        if a { break }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK-NOT: destroy_value [[X]]
        // CHECK: br

        if a { return }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: destroy_value [[MARKED_X]]
        // CHECK: br [[EPILOG]]

        var y:Int
        // CHECK: [[Y:%[0-9]+]] = alloc_box ${ var Int }
        // CHECK: [[MARKED_Y:%.*]] = mark_uninitialized [var] [[Y]]

        if a { break }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: destroy_value [[MARKED_Y]]
        // CHECK-NOT: destroy_value [[MARKED_X]]
        // CHECK-NOT: destroy_value [[A]]
        // CHECK: br

        if a { return }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: destroy_value [[MARKED_Y]]
        // CHECK: destroy_value [[MARKED_X]]
        // CHECK: br [[EPILOG]]

        if true {
            var z:Int
            // CHECK: [[Z:%[0-9]+]] = alloc_box ${ var Int }
            // CHECK: [[MARKED_Z:%.*]] = mark_uninitialized [var] [[Z]]

            if a { break }
            // CHECK: cond_br
            // CHECK: {{bb.*:}}
            // CHECK: destroy_value [[MARKED_Z]]
            // CHECK: destroy_value [[MARKED_Y]]
            // CHECK-NOT: destroy_value [[MARKED_X]]
            // CHECK-NOT: destroy_value [[A]]
            // CHECK: br

            if a { return }
            // CHECK: cond_br
            // CHECK: {{bb.*:}}
            // CHECK: destroy_value [[MARKED_Z]]
            // CHECK: destroy_value [[MARKED_Y]]
            // CHECK: destroy_value [[MARKED_X]]
            // CHECK: br [[EPILOG]]

            // CHECK: destroy_value [[MARKED_Z]]
        }
        if a { break }
        // CHECK: cond_br
        // CHECK: {{bb.*:}}
        // CHECK: destroy_value [[MARKED_Y]]
        // CHECK-NOT: destroy_value [[MARKED_X]]
        // CHECK-NOT: destroy_value [[A]]
        // CHECK: br

        // CHECK: {{bb.*:}}
        // CHECK: destroy_value [[MARKED_Y]]
        // CHECK: br
    }
    // CHECK: destroy_value [[MARKED_X]]
    // CHECK: [[EPILOG]]:
    // CHECK: return
}

func reftype_func() -> Ref {}
func reftype_func_with_arg(_ x: Ref) -> Ref {}

// CHECK-LABEL: sil hidden @$S8lifetime14reftype_returnAA3RefCyF
func reftype_return() -> Ref {
    return reftype_func()
    // CHECK: [[RF:%[0-9]+]] = function_ref @$S8lifetime12reftype_funcAA3RefCyF : $@convention(thin) () -> @owned Ref
    // CHECK-NOT: destroy_value
    // CHECK: [[RET:%[0-9]+]] = apply [[RF]]()
    // CHECK-NOT: destroy_value
    // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @$S8lifetime11reftype_argyyAA3RefCF : $@convention(thin) (@guaranteed Ref) -> () {
// CHECK: bb0([[A:%[0-9]+]] : $Ref):
// CHECK:   [[AADDR:%[0-9]+]] = alloc_box ${ var Ref }
// CHECK:   [[PA:%[0-9]+]] = project_box [[AADDR]]
// CHECK:   [[A_COPY:%.*]] = copy_value [[A]]
// CHECK:   store [[A_COPY]] to [init] [[PA]]
// CHECK:   destroy_value [[AADDR]]
// CHECK-NOT:   destroy_value [[A]]
// CHECK:   return
// CHECK: } // end sil function '$S8lifetime11reftype_argyyAA3RefCF'
func reftype_arg(_ a: Ref) {
    var a = a
}

// CHECK-LABEL: sil hidden @$S8lifetime26reftype_call_ignore_returnyyF
func reftype_call_ignore_return() {
    reftype_func()
    // CHECK: = function_ref @$S8lifetime12reftype_funcAA3RefCyF : $@convention(thin) () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK: destroy_value [[R]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @$S8lifetime27reftype_call_store_to_localyyF
func reftype_call_store_to_local() {
    var a = reftype_func()
    // CHECK: [[A:%[0-9]+]] = alloc_box ${ var Ref }
    // CHECK-NEXT: [[PB:%.*]] = project_box [[A]]
    // CHECK: = function_ref @$S8lifetime12reftype_funcAA3RefCyF : $@convention(thin) () -> @owned Ref
    // CHECK-NEXT: [[R:%[0-9]+]] = apply
    // CHECK-NOT: copy_value [[R]]
    // CHECK: store [[R]] to [init] [[PB]]
    // CHECK-NOT: destroy_value [[R]]
    // CHECK: destroy_value [[A]]
    // CHECK-NOT: destroy_value [[R]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @$S8lifetime16reftype_call_argyyF
func reftype_call_arg() {
    reftype_func_with_arg(reftype_func())
    // CHECK: [[RF:%[0-9]+]] = function_ref @$S8lifetime12reftype_func{{[_0-9a-zA-Z]*}}F
    // CHECK: [[R1:%[0-9]+]] = apply [[RF]]
    // CHECK: [[RFWA:%[0-9]+]] = function_ref @$S8lifetime21reftype_func_with_arg{{[_0-9a-zA-Z]*}}F
    // CHECK: [[R2:%[0-9]+]] = apply [[RFWA]]([[R1]])
    // CHECK: destroy_value [[R2]]
    // CHECK: return
}

// CHECK-LABEL: sil hidden @$S8lifetime21reftype_call_with_arg{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[A1:%[0-9]+]] : $Ref):
// CHECK:   [[AADDR:%[0-9]+]] = alloc_box ${ var Ref }
// CHECK:   [[PB:%.*]] = project_box [[AADDR]]
// CHECK:   [[A1_COPY:%.*]] = copy_value [[A1]]
// CHECK:   store [[A1_COPY]] to [init] [[PB]]
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]]
// CHECK:   [[A2:%[0-9]+]] = load [copy] [[READ]]
// CHECK:   [[RFWA:%[0-9]+]] = function_ref @$S8lifetime21reftype_func_with_arg{{[_0-9a-zA-Z]*}}F
// CHECK:   [[RESULT:%.*]] = apply [[RFWA]]([[A2]])
// CHECK:   destroy_value [[RESULT]]
// CHECK:   destroy_value [[AADDR]]
// CHECK-NOT:   destroy_value [[A1]]
// CHECK:   return
func reftype_call_with_arg(_ a: Ref) {
    var a = a

    reftype_func_with_arg(a)
}

// CHECK-LABEL: sil hidden @$S8lifetime16reftype_reassign{{[_0-9a-zA-Z]*}}F
func reftype_reassign(_ a: inout Ref, b: Ref) {
    var b = b
    // CHECK: bb0([[AADDR:%[0-9]+]] : $*Ref, [[B1:%[0-9]+]] : $Ref):
    // CHECK: [[BADDR:%[0-9]+]] = alloc_box ${ var Ref }
    // CHECK: [[PBB:%.*]] = project_box [[BADDR]]
    a = b
    // CHECK: destroy_value

    // CHECK: return
}

func tuple_with_ref_elements() -> (Val, (Ref, Val), Ref) {}

// CHECK-LABEL: sil hidden @$S8lifetime28tuple_with_ref_ignore_returnyyF
func tuple_with_ref_ignore_return() {
  tuple_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$S8lifetime23tuple_with_ref_elementsAA3ValV_AA3RefC_ADtAFtyF
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[BORROWED_TUPLE:%.*]] = begin_borrow [[TUPLE]]
  // CHECK: [[T0:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 0
  // CHECK: [[T1_0:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 1
  // CHECK: [[T1_0_COPY:%.*]] = copy_value [[T1_0]]
  // CHECK: [[T1_1:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 2
  // CHECK: [[T2:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 3
  // CHECK: [[T2_COPY:%.*]] = copy_value [[T2]]
  // CHECK: end_borrow [[BORROWED_TUPLE]] from [[TUPLE]]
  // CHECK: destroy_value [[TUPLE]]
  // CHECK: destroy_value [[T2_COPY]]
  // CHECK: destroy_value [[T1_0_COPY]]
  // CHECK: return
}

struct Aleph {
  var a:Ref
  var b:Val

  // -- loadable value constructor:
  // CHECK-LABEL: sil hidden @$S8lifetime5AlephV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned Ref, Val, @thin Aleph.Type) -> @owned Aleph
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
  // CHECK-LABEL: sil hidden @$S8lifetime6DalethV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned Aleph, @owned Beth, @in Unloadable, @thin Daleth.Type) -> @out Daleth {
  // CHECK: bb0([[THIS:%.*]] : $*Daleth, [[A:%.*]] : $Aleph, [[B:%.*]] : $Beth, [[C:%.*]] : $*Unloadable, {{%.*}} : $@thin Daleth.Type):
  // CHECK-NEXT:   [[A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.a
  // CHECK-NEXT:   store [[A]] to [init] [[A_ADDR]]
  // CHECK-NEXT:   [[B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.b
  // CHECK-NEXT:   store [[B]] to [init] [[B_ADDR]]
  // CHECK-NEXT:   [[C_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Daleth, #Daleth.c
  // CHECK-NEXT:   copy_addr [take] [[C]] to [initialization] [[C_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

class He {
  
  // -- default allocator:
  // CHECK-LABEL: sil hidden @$S8lifetime2HeC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick He.Type) -> @owned He {
  // CHECK: bb0({{%.*}} : $@thick He.Type):
  // CHECK-NEXT:   [[THIS:%.*]] = alloc_ref $He
  // CHECK-NEXT:   // function_ref lifetime.He.init
  // CHECK-NEXT:   [[INIT:%.*]] = function_ref @$S8lifetime2HeC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned He) -> @owned He
  // CHECK-NEXT:   [[THIS1:%.*]] = apply [[INIT]]([[THIS]])
  // CHECK-NEXT:   return [[THIS1]]
  // CHECK-NEXT: }

  // -- default initializer:
  // CHECK-LABEL: sil hidden @$S8lifetime2HeC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned He) -> @owned He {
  // CHECK: bb0([[SELF:%.*]] : $He):
  // CHECK-NEXT: debug_value
  // CHECK-NEXT: [[UNINITIALIZED_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]]
  // CHECK-NEXT: [[UNINITIALIZED_SELF_COPY:%.*]] = copy_value [[UNINITIALIZED_SELF]]
  // CHECK-NEXT: destroy_value [[UNINITIALIZED_SELF]]
  // CHECK-NEXT: return [[UNINITIALIZED_SELF_COPY]]
  // CHECK: } // end sil function '$S8lifetime2HeC{{[_0-9a-zA-Z]*}}fc'

  init() { }
}

struct Waw {
  var a:(Ref, Val)
  var b:Val

  // -- loadable value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @$S8lifetime3WawV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned Ref, Val, Val, @thin Waw.Type) -> @owned Waw 
  // CHECK: bb0([[A0:%.*]] : $Ref, [[A1:%.*]] : $Val, [[B:%.*]] : $Val, {{%.*}} : $@thin Waw.Type):
  // CHECK-NEXT:   [[A:%.*]] = tuple ([[A0]] : {{.*}}, [[A1]] : {{.*}})
  // CHECK-NEXT:   [[RET:%.*]] = struct $Waw ([[A]] : {{.*}}, [[B]] : {{.*}})
  // CHECK-NEXT:   return [[RET]]
}

struct Zayin {
  var a:(Unloadable, Val)
  var b:Unloadable

  // -- address-only value initializer with tuple destructuring:
  // CHECK-LABEL: sil hidden @$S8lifetime5ZayinV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@in Unloadable, Val, @in Unloadable, @thin Zayin.Type) -> @out Zayin
  // CHECK: bb0([[THIS:%.*]] : $*Zayin, [[A0:%.*]] : $*Unloadable, [[A1:%.*]] : $Val, [[B:%.*]] : $*Unloadable, {{%.*}} : $@thin Zayin.Type):
  // CHECK-NEXT:   [[THIS_A_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #Zayin.a
  // CHECK-NEXT:   [[THIS_A0_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 0
  // CHECK-NEXT:   [[THIS_A1_ADDR:%.*]] = tuple_element_addr [[THIS_A_ADDR]] : {{.*}}, 1
  // CHECK-NEXT:   copy_addr [take] [[A0]] to [initialization] [[THIS_A0_ADDR]]
  // CHECK-NEXT:   store [[A1]] to [trivial] [[THIS_A1_ADDR]]
  // CHECK-NEXT:   [[THIS_B_ADDR:%.*]] = struct_element_addr [[THIS]] : $*Zayin, #Zayin.b
  // CHECK-NEXT:   copy_addr [take] [[B]] to [initialization] [[THIS_B_ADDR]]
  // CHECK-NEXT:   tuple ()
  // CHECK-NEXT:   return
}

func fragile_struct_with_ref_elements() -> Beth {}

// CHECK-LABEL: sil hidden @$S8lifetime29struct_with_ref_ignore_returnyyF
func struct_with_ref_ignore_return() {
  fragile_struct_with_ref_elements()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$S8lifetime32fragile_struct_with_ref_elementsAA4BethVyF
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: destroy_value [[STRUCT]] : $Beth
  // CHECK: return
}

// CHECK-LABEL: sil hidden @$S8lifetime28struct_with_ref_materializedyyF
func struct_with_ref_materialized() {
  fragile_struct_with_ref_elements().gimel()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$S8lifetime32fragile_struct_with_ref_elementsAA4BethVyF
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]
  // CHECK: [[METHOD:%[0-9]+]] = function_ref @$S8lifetime4BethV5gimel{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[METHOD]]([[STRUCT]])
}

class RefWithProp {
  var int_prop: Int { get {} set {} }
  var aleph_prop: Aleph { get {} set {} }
}

// CHECK-LABEL: sil hidden @$S8lifetime015logical_lvalue_A0yyAA11RefWithPropC_SiAA3ValVtF : $@convention(thin) (@guaranteed RefWithProp, Int, Val) -> () {
func logical_lvalue_lifetime(_ r: RefWithProp, _ i: Int, _ v: Val) {
  var r = r
  var i = i
  var v = v
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box ${ var RefWithProp }
  // CHECK: [[PR:%[0-9]+]] = project_box [[RADDR]]
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PI:%[0-9]+]] = project_box [[IADDR]]
  // CHECK: store %1 to [trivial] [[PI]]
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box ${ var Val }
  // CHECK: [[PV:%[0-9]+]] = project_box [[VADDR]]

  // -- Reference types need to be copy_valued as property method args.
  r.int_prop = i
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PR]]
  // CHECK: [[R1:%[0-9]+]] = load [copy] [[READ]]
  // CHECK: [[SETTER_METHOD:%[0-9]+]] = class_method {{.*}} : $RefWithProp, #RefWithProp.int_prop!setter.1 : (RefWithProp) -> (Int) -> (), $@convention(method) (Int, @guaranteed RefWithProp) -> ()
  // CHECK: apply [[SETTER_METHOD]]({{.*}}, [[R1]])
  // CHECK: destroy_value [[R1]]

  r.aleph_prop.b = v
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PR]]
  // CHECK: [[R2:%[0-9]+]] = load [copy] [[READ]]
  // CHECK: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: [[ALEPH_PROP_TEMP:%[0-9]+]] = alloc_stack $Aleph
  // CHECK: [[BORROWED_R2:%.*]] = begin_borrow [[R2]]
  // CHECK: [[T0:%.*]] = address_to_pointer [[ALEPH_PROP_TEMP]]
  // CHECK: [[MATERIALIZE_METHOD:%[0-9]+]] = class_method [[BORROWED_R2]] : $RefWithProp, #RefWithProp.aleph_prop!materializeForSet.1 :
  // CHECK: [[MATERIALIZE:%.*]] = apply [[MATERIALIZE_METHOD]]([[T0]], [[STORAGE]], [[BORROWED_R2]])
  // CHECK: [[PTR:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 0
  // CHECK: [[OPTCALLBACK:%.*]] = tuple_extract [[MATERIALIZE]] : {{.*}}, 1
  // CHECK: [[ADDR:%.*]] = pointer_to_address [[PTR]]
  // CHECK: [[MARKED_ADDR:%.*]] = mark_dependence [[ADDR]] : $*Aleph on [[R2]]
  // CHECK: {{.*}}([[CALLBACK_ADDR:%.*]] : 
  // CHECK: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed RefWithProp, @thick RefWithProp.Type) -> ()
  // CHECK: [[TEMP:%.*]] = alloc_stack $RefWithProp
  // CHECK: store [[R2]] to [init] [[TEMP]]
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
  // -- allocating entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fC :
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fc
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_METHOD]]<{{.*}}>([[THIS]])
    // CHECK: return [[INIT_THIS]]

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fc :
    // CHECK: bb0([[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK: [[THIS:%[0-9]+]] = mark_uninitialized

    // -- initialization for y
    // CHECK: [[Y_INIT:%[0-9]+]] = function_ref @$S8lifetime3FooC1ySi_AA3RefCtvpfi : $@convention(thin) <τ_0_0> () -> (Int, @owned Ref)
    // CHECK: [[Y_VALUE:%[0-9]+]] = apply [[Y_INIT]]<T>()
    // CHECK: [[BORROWED_Y_VALUE:%.*]] = begin_borrow [[Y_VALUE]]
    // CHECK: [[Y_EXTRACTED_0:%.*]] = tuple_extract [[BORROWED_Y_VALUE]] : $(Int, Ref), 0
    // CHECK: [[Y_EXTRACTED_1:%.*]] = tuple_extract [[BORROWED_Y_VALUE]] : $(Int, Ref), 1
    // CHECK: [[COPIED_Y_EXTRACTED_1:%.*]] = copy_value [[Y_EXTRACTED_1]]
    // CHECK: end_borrow [[BORROWED_Y_VALUE]] from [[Y_VALUE]]
    // CHECK: destroy_value [[Y_VALUE]]
    // CHECK: [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK: [[THIS_Y:%.*]] = ref_element_addr [[BORROWED_THIS]] : {{.*}}, #Foo.y
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_Y]] : $*(Int, Ref)
    // CHECK: [[THIS_Y_0:%.*]] = tuple_element_addr [[WRITE]] : $*(Int, Ref), 0
    // CHECK: assign [[Y_EXTRACTED_0]] to [[THIS_Y_0]]
    // CHECK: [[THIS_Y_1:%.*]] = tuple_element_addr [[WRITE]] : $*(Int, Ref), 1
    // CHECK: assign [[COPIED_Y_EXTRACTED_1]] to [[THIS_Y_1]]
    // CHECK: end_access [[WRITE]] : $*(Int, Ref)
    // CHECK: end_borrow [[BORROWED_THIS]] from [[THIS]]

    // -- Initialization for w
    // CHECK: [[Z_FUNC:%.*]] = function_ref @$S{{.*}}8lifetime3FooC1wAA3RefCvpfi : $@convention(thin) <τ_0_0> () -> @owned Ref
    // CHECK: [[Z_RESULT:%.*]] = apply [[Z_FUNC]]<T>()
    // CHECK: [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK: [[THIS_Z:%.*]] = ref_element_addr [[BORROWED_THIS]]
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_Z]] : $*Ref
    // CHECK: assign [[Z_RESULT]] to [[WRITE]]
    // CHECK: end_borrow [[BORROWED_THIS]] from [[THIS]]

    // -- Initialization for x
    // CHECK: [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK: [[THIS_X:%[0-9]+]] = ref_element_addr [[BORROWED_THIS]] : {{.*}}, #Foo.x
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_X]] : $*Int
    // CHECK: assign {{.*}} to [[WRITE]]
    // CHECK: end_borrow [[BORROWED_THIS]] from [[THIS]]

    x = bar()

    z = Foo<T>.makeT()
    // CHECK: [[FOOMETA:%[0-9]+]] = metatype $@thick Foo<T>.Type
    // CHECK: [[MAKET:%[0-9]+]] = class_method [[FOOMETA]] : {{.*}}, #Foo.makeT!1
    // CHECK: ref_element_addr

    // -- cleanup this lvalue and return this
    // CHECK: [[THIS_RESULT:%.*]] = copy_value [[THIS]]
    // -- TODO: This copy should be unnecessary.
    // CHECK: destroy_value [[THIS]]
    // CHECK: return [[THIS_RESULT]]

  }

  init(chi:Int) {
    var chi = chi
    z = Foo<T>.makeT()

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fC :
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[METATYPE:%[0-9]+]] : $@thick Foo<T>.Type):
    // CHECK: [[THIS:%[0-9]+]] = alloc_ref $Foo<T>
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fc
    // CHECK: [[INIT_THIS:%[0-9]+]] = apply [[INIT_METHOD]]<{{.*}}>([[CHI]], [[THIS]])
    // CHECK: return [[INIT_THIS]]

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC3chiACyxGSi_tcfc : $@convention(method) <T> (Int, @owned Foo<T>) -> @owned Foo<T> {
    // CHECK: bb0([[CHI:%[0-9]+]] : $Int, [[THISIN:%[0-9]+]] : $Foo<T>):
    // CHECK:   [[THIS:%[0-9]+]] = mark_uninitialized [rootself] [[THISIN]]

    // -- First we initialize #Foo.y.
    // CHECK:   [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK:   [[THIS_Y:%.*]] = ref_element_addr [[BORROWED_THIS]] : $Foo<T>, #Foo.y
    // CHECK:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_Y]] : $*(Int, Ref)
    // CHECK:   [[THIS_Y_1:%.*]] = tuple_element_addr [[WRITE]] : $*(Int, Ref), 0
    // CHECK:   assign {{.*}} to [[THIS_Y_1]] : $*Int
    // CHECK:   [[THIS_Y_2:%.*]] = tuple_element_addr [[WRITE]] : $*(Int, Ref), 1
    // CHECK:   assign {{.*}} to [[THIS_Y_2]] : $*Ref
    // CHECK:   end_borrow [[BORROWED_THIS]] from [[THIS]]

    // -- Then we create a box that we will use to perform a copy_addr into #Foo.x a bit later.
    // CHECK:   [[CHIADDR:%[0-9]+]] = alloc_box ${ var Int }, var, name "chi"
    // CHECK:   [[PCHI:%[0-9]+]] = project_box [[CHIADDR]]
    // CHECK:   store [[CHI]] to [trivial] [[PCHI]]

    // -- Then we initialize #Foo.z
    // CHECK:   [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK:   [[THIS_Z:%.*]] = ref_element_addr [[BORROWED_THIS]] : {{.*}}, #Foo.z
    // CHECK:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_Z]] : $*T
    // CHECK:   copy_addr [take] {{.*}} to [[WRITE]]
    // CHECK:   end_borrow [[BORROWED_THIS]] from [[THIS]]

    // -- Then initialize #Foo.x using the earlier stored value of CHI to THIS_Z.
    x = chi
    // CHECK:   [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
    // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PCHI]]
    // CHECK:   [[X:%.*]] = load [trivial] [[READ]]
    // CHECK:   [[THIS_X:%[0-9]+]] = ref_element_addr [[BORROWED_THIS]] : {{.*}}, #Foo.x
    // CHECK:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[THIS_X]] : $*Int
    // CHECK:   assign [[X]] to [[WRITE]]
    // CHECK:   end_borrow [[BORROWED_THIS]] from [[THIS]]

    // -- cleanup chi
    // CHECK: destroy_value [[CHIADDR]]

    // -- Then begin the epilogue sequence
    // CHECK: [[THIS_RETURN:%.*]] = copy_value [[THIS]]
    // CHECK: destroy_value [[THIS]]
    // CHECK: return [[THIS_RETURN]]
  // CHECK: } // end sil function '$S8lifetime3FooC3chiACyxGSi_tcfc'
  }

  // -- allocating entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fC :
    // CHECK: [[INIT_METHOD:%[0-9]+]] = function_ref @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fc

  // -- initializing entry point
  // CHECK-LABEL: sil hidden @$S8lifetime3FooC{{[_0-9a-zA-Z]*}}fc :

  init<U:Intifiable>(chi:U) {
    z = Foo<T>.makeT()

    x = chi.intify()
  }
  
  // CHECK-LABEL: sil hidden @$S8lifetime3FooCfd : $@convention(method) <T> (@guaranteed Foo<T>) -> @owned Builtin.NativeObject

  deinit {
    // CHECK: bb0([[THIS:%[0-9]+]] : $Foo<T>):
    bar()
    // CHECK: function_ref @$S8lifetime3barSiyF
    // CHECK: apply

    // -- don't need to destroy_value x because it's trivial
    // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #Foo.x
    // -- destroy_value y
    // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.y
    // CHECK: destroy_addr [[YADDR]]
    // -- destroy_value z
    // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.z
    // CHECK: destroy_addr [[ZADDR]]
    // -- destroy_value w
    // CHECK: [[WADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #Foo.w
    // CHECK: destroy_addr [[WADDR]]
    // -- return back this
    // CHECK: [[PTR:%.*]] = unchecked_ref_cast [[THIS]] : $Foo<T> to $Builtin.NativeObject
    // CHECK: [[PTR_OWNED:%.*]] = unchecked_ownership_conversion [[PTR]] : $Builtin.NativeObject, @guaranteed to @owned
    // CHECK: return [[PTR_OWNED]]
    // CHECK: } // end sil function '$S8lifetime3FooCfd'
  }

  // Deallocating destructor for Foo.
  // CHECK-LABEL: sil hidden @$S8lifetime3FooCfD : $@convention(method) <T> (@owned Foo<T>) -> ()
  // CHECK: bb0([[SELF:%[0-9]+]] : $Foo<T>):
  // CHECK:   [[DESTROYING_REF:%[0-9]+]] = function_ref @$S8lifetime3FooCfd : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK-NEXT:   [[RESULT_SELF:%[0-9]+]] = apply [[DESTROYING_REF]]<T>([[BORROWED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  // CHECK-NEXT:   end_lifetime [[SELF]]
  // CHECK-NEXT:   [[SELF:%[0-9]+]] = unchecked_ref_cast [[RESULT_SELF]] : $Builtin.NativeObject to $Foo<T>
  // CHECK-NEXT:   dealloc_ref [[SELF]] : $Foo<T>
  // CHECK-NEXT:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT:   return [[RESULT]] : $()
  // CHECK-NEXT: } // end sil function '$S8lifetime3FooCfD'

}

class FooSubclass<T> : Foo<T> {

  // CHECK-LABEL: sil hidden @$S8lifetime11FooSubclassCfd : $@convention(method) <T> (@guaranteed FooSubclass<T>) -> @owned Builtin.NativeObject
  // CHECK: bb0([[THIS:%[0-9]+]] : $FooSubclass<T>):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $Foo<T>
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @$S8lifetime3FooCfd : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK: [[PTR:%.*]] = apply [[BASE_DTOR]]<T>([[BASE]])
  // CHECK: [[BORROWED_PTR:%.*]] = begin_borrow [[PTR]]
  // CHECK: end_borrow [[BORROWED_PTR]] from [[PTR]]
  // CHECK: return [[PTR]]
  

  deinit {
    bar()
  }
}

class ImplicitDtor {
  var x:Int
  var y:(Int, Ref)
  var w:Ref
  init() { }

  // CHECK-LABEL: sil hidden @$S8lifetime12ImplicitDtorCfd
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtor):
  // -- don't need to destroy_value x because it's trivial
  // CHECK-NOT: ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtor.x
  // -- destroy_value y
  // CHECK: [[YADDR:%[0-9]+]] = ref_element_addr [[THIS]] : {{.*}}, #ImplicitDtor.y
  // CHECK: destroy_addr [[YADDR]]
  // -- destroy_value w
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

  // CHECK: sil hidden @$S8lifetime19ImplicitDtorDerivedCfd : $@convention(method) <T> (@guaranteed ImplicitDtorDerived<T>) -> @owned Builtin.NativeObject {
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerived<T>):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtor
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @$S8lifetime12ImplicitDtorCfd
  // CHECK: [[PTR:%.*]] = apply [[BASE_DTOR]]([[BASE]])
  // -- destroy_value z
  // CHECK: [[BORROWED_PTR:%.*]] = begin_borrow [[PTR]]
  // CHECK: [[CAST_BORROWED_PTR:%.*]] = unchecked_ref_cast [[BORROWED_PTR]] : $Builtin.NativeObject to $ImplicitDtorDerived<T>
  // CHECK: [[ZADDR:%[0-9]+]] = ref_element_addr [[CAST_BORROWED_PTR]] : {{.*}}, #ImplicitDtorDerived.z
  // CHECK: destroy_addr [[ZADDR]]
  // CHECK: end_borrow [[BORROWED_PTR]] from [[PTR]]
  // -- epilog
  // CHECK-NOT: unchecked_ref_cast
  // CHECK-NOT: unchecked_ownership_conversion
  // CHECK: return [[PTR]]
}

class ImplicitDtorDerivedFromGeneric<T> : ImplicitDtorDerived<Int> {
  init() { super.init(z: 5) }

  // CHECK-LABEL: sil hidden @$S8lifetime30ImplicitDtorDerivedFromGenericC{{[_0-9a-zA-Z]*}}fc
  // CHECK: bb0([[THIS:%[0-9]+]] : $ImplicitDtorDerivedFromGeneric<T>):
  // -- base dtor
  // CHECK: [[BASE:%[0-9]+]] = upcast [[THIS]] : ${{.*}} to $ImplicitDtorDerived<Int>
  // CHECK: [[BASE_DTOR:%[0-9]+]] = function_ref @$S8lifetime19ImplicitDtorDerivedCfd
  // CHECK: [[PTR:%.*]] = apply [[BASE_DTOR]]<Int>([[BASE]])
  // CHECK: return [[PTR]]
}

protocol Intifiable {
  func intify() -> Int
}

struct Bar {
  var x:Int

  // Loadable struct initializer
  // CHECK-LABEL: sil hidden @$S8lifetime3BarV{{[_0-9a-zA-Z]*}}fC
  init() {
    // CHECK: bb0([[METATYPE:%[0-9]+]] : $@thin Bar.Type):
    // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Bar }
    // CHECK: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_BOX]]
    // CHECK: [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]

    x = bar()
    // CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_BOX]]
    // CHECK: [[SELF_X:%[0-9]+]] = struct_element_addr [[WRITE]] : $*Bar, #Bar.x
    // CHECK: assign {{.*}} to [[SELF_X]]

    // -- load and return this
    // CHECK: [[SELF_VAL:%[0-9]+]] = load [trivial] [[PB_BOX]]
    // CHECK: destroy_value [[MARKED_SELF_BOX]]
    // CHECK: return [[SELF_VAL]]
  }

  init<T:Intifiable>(xx:T) {
    x = xx.intify()
  }
}

struct Bas<T> {
  var x:Int
  var y:T

  // Address-only struct initializer
  // CHECK-LABEL: sil hidden @$S8lifetime3BasV{{[_0-9a-zA-Z]*}}fC
  init(yy:T) {
    // CHECK: bb0([[THISADDRPTR:%[0-9]+]] : $*Bas<T>, [[YYADDR:%[0-9]+]] : $*T, [[META:%[0-9]+]] : $@thin Bas<T>.Type):
    // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Bas<τ_0_0> } <T>
    // CHECK: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_BOX]]
    // CHECK: [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]

    x = bar()
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_BOX]]
    // CHECK: [[SELF_X:%[0-9]+]] = struct_element_addr [[WRITE]] : $*Bas<T>, #Bas.x
    // CHECK: assign {{.*}} to [[SELF_X]]

    y = yy
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_BOX]]
    // CHECK: [[SELF_Y:%[0-9]+]] = struct_element_addr [[WRITE]] : $*Bas<T>, #Bas.y
    // CHECK: copy_addr {{.*}} to [[SELF_Y]]
    // CHECK: destroy_value

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
  // CHECK-LABEL: sil hidden @$S8lifetime1DC1x1yACSi_Sitcfc
  // CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Int, [[SELF:%[0-9]+]] : $D):
  init(x: Int, y: Int) {
    var x = x
    var y = y
    // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var D }
    // CHECK: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [derivedself] [[SELF_BOX]]
    // CHECK: [[PB_BOX:%[0-9]+]] = project_box [[MARKED_SELF_BOX]]
    // CHECK: store [[SELF]] to [init] [[PB_BOX]]
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
    // CHECK: [[PX:%[0-9]+]] = project_box [[XADDR]]
    // CHECK: store [[X]] to [trivial] [[PX]]
    // CHECK: [[YADDR:%[0-9]+]] = alloc_box ${ var Int }
    // CHECK: [[PY:%[0-9]+]] = project_box [[YADDR]]
    // CHECK: store [[Y]] to [trivial] [[PY]]

    super.init(y: y)
    // CHECK: [[THIS1:%[0-9]+]] = load [take] [[PB_BOX]]
    // CHECK: [[THIS1_SUP:%[0-9]+]] = upcast [[THIS1]] : ${{.*}} to $B
    // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PY]]
    // CHECK: [[Y:%[0-9]+]] = load [trivial] [[READ]]
    // CHECK: [[SUPER_CTOR:%[0-9]+]] = function_ref @$S8lifetime1BC1yACSi_tcfc : $@convention(method) (Int, @owned B) -> @owned B
    // CHECK: [[THIS2_SUP:%[0-9]+]] = apply [[SUPER_CTOR]]([[Y]], [[THIS1_SUP]])
    // CHECK: [[THIS2:%[0-9]+]] = unchecked_ref_cast [[THIS2_SUP]] : $B to $D
    // CHECK: [[THIS1:%[0-9]+]] = load [copy] [[PB_BOX]]
    // CHECK: destroy_value [[MARKED_SELF_BOX]]
  }

  func foo() {}
}

// CHECK-LABEL: sil hidden @$S8lifetime8downcast{{[_0-9a-zA-Z]*}}F
func downcast(_ b: B) {
  var b = b
  // CHECK: [[BADDR:%[0-9]+]] = alloc_box ${ var B }
  // CHECK: [[PB:%[0-9]+]] = project_box [[BADDR]]
  (b as! D).foo()
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]]
  // CHECK: [[B:%[0-9]+]] = load [copy] [[READ]]
  // CHECK: [[D:%[0-9]+]] = unconditional_checked_cast [[B]] : {{.*}} to $D
  // CHECK: apply {{.*}}([[D]])
  // CHECK-NOT: destroy_value [[B]]
  // CHECK: destroy_value [[D]]
  // CHECK: destroy_value [[BADDR]]
  // CHECK: return
}

func int(_ x: Int) {}
func ref(_ x: Ref) {}

func tuple() -> (Int, Ref) { return (1, Ref()) }

func tuple_explosion() {
  int(tuple().0)
  // CHECK: [[F:%[0-9]+]] = function_ref @$S8lifetime5tupleSi_AA3RefCtyF
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[BORROWED_TUPLE:%.*]] = begin_borrow [[TUPLE]]
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 1
  // CHECK: [[T1_COPY:%.*]] = copy_value [[T1]]
  // CHECK: end_borrow [[BORROWED_TUPLE]] from [[TUPLE]]
  // CHECK: destroy_value [[T1_COPY]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: destroy_value

  ref(tuple().1)
  // CHECK: [[F:%[0-9]+]] = function_ref @$S8lifetime5tupleSi_AA3RefCtyF
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[F]]()
  // CHECK: [[BORROWED_TUPLE:%.*]] = begin_borrow [[TUPLE]]
  // CHECK: [[T1:%[0-9]+]] = tuple_extract [[BORROWED_TUPLE]] : {{.*}}, 1
  // CHECK: [[T1_COPY:%.*]] = copy_value [[T1]]
  // CHECK: end_borrow [[BORROWED_TUPLE]] from [[TUPLE]]
  // CHECK: destroy_value [[TUPLE]]
  // CHECK-NOT: destroy_value [[T1]]
  // CHECK-NOT: tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK-NOT: destroy_value [[TUPLE]]
}

class C {
  var v = ""
  // CHECK-LABEL: sil hidden @$S8lifetime1CC18ignored_assignment{{[_0-9a-zA-Z]*}}F
  func ignored_assignment() {
    // CHECK: [[STRING:%.*]] = alloc_stack $String
    // CHECK: [[UNINIT:%.*]] = mark_uninitialized [var] [[STRING]]
    // CHECK: assign {{%.*}} to [[UNINIT]]
    // CHECK: destroy_addr [[UNINIT]]
    _ = self.v
  }
}
