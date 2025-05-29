
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name let_decls -Xllvm -sil-full-demangle %s | %FileCheck %s

func takeClosure(_ a : () -> Int) {}

// Let decls don't get boxes for trivial types.
//
// CHECK-LABEL: sil hidden [ossa] @{{.*}}test1
func test1(_ a : Int) -> Int {
  // CHECK-NOT: alloc_box
  let (b,c) = (a, 32)

  return b+c
  
  // CHECK: return
}

// rdar://15716277
// CHECK: @{{.*}}_destructuring
func let_destructuring() -> Int {
  let (a, b) = ((1,2), 5)
  return a.1+a.0+b
}

// Let decls being closed over.
//
// CHECK-LABEL: sil hidden [ossa] @{{.*}}test2
func test2() {
  // No allocations.
  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_stack

  let x = 42

  takeClosure({x})


  // CHECK: return
}

// The closure just returns its value, which it captured directly.

// CHECK: sil private [ossa] @$s9let_decls5test2yyFSiyXEfU_ : $@convention(thin) (Int) -> Int
// CHECK: bb0(%0 : @closureCapture $Int):
// CHECK:  return %0 : $Int

// Verify that we can close over let decls of tuple type.
struct RegularStruct {
  var a: Int
}
func testTupleLetCapture() {
  let t = (RegularStruct(a: 41), 42)

  takeClosure( { t.0.a })
}



func getAString() -> String { return "" }
func useAString(_ a : String) {}

// rdar://15689514 - Verify that the cleanup for the let decl runs at the end of
// the 'let' lifetime, not at the end of the initializing expression.
//
// CHECK-LABEL: sil hidden [ossa] @{{.*}}test3
func test3() {
  // CHECK: [[GETFN:%[0-9]+]] = function_ref{{.*}}getAString
  // CHECK-NEXT: [[STR:%[0-9]+]] = apply [[GETFN]]()
  let o = getAString()

  // CHECK-NEXT: [[STR_MOV:%.*]] = move_value [var_decl] [[STR]]
  // CHECK-NEXT: debug_value
  // CHECK-NOT: destroy_value

  // CHECK: [[STR_BORROW:%.*]] = begin_borrow [[STR_MOV]]
  // CHECK: [[USEFN:%[0-9]+]] = function_ref{{.*}}useAString
  // CHECK-NEXT: [[USE:%[0-9]+]] = apply [[USEFN]]([[STR_BORROW]])
  useAString(o)
  
  // CHECK: destroy_value [[STR_MOV]]
}
// CHECK: } // end sil function '{{.*}}test3{{.*}}'



struct AddressOnlyStruct<T> {
  var elt : T
  var str : String
}

func produceAddressOnlyStruct<T>(_ x : T) -> AddressOnlyStruct<T> {}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testAddressOnlyStructString
// CHECK: bb0([[FUNC_ARG:%.*]] : $*T):
func testAddressOnlyStructString<T>(_ a : T) -> String {
  return produceAddressOnlyStruct(a).str
  
  // CHECK: [[TMPSTRUCT:%[0-9]+]] = alloc_stack $AddressOnlyStruct<T>
  // CHECK: [[PRODFN:%[0-9]+]] = function_ref @{{.*}}produceAddressOnlyStruct
  // CHECK: apply [[PRODFN]]<T>([[TMPSTRUCT]], [[FUNC_ARG]])
  // CHECK-NEXT: [[STRADDR:%[0-9]+]] = struct_element_addr [[TMPSTRUCT]] : $*AddressOnlyStruct<T>, #AddressOnlyStruct.str
  // CHECK-NEXT: [[STRVAL:%[0-9]+]] = load [copy] [[STRADDR]]
  // CHECK-NEXT: destroy_addr [[TMPSTRUCT]]
  // CHECK-NEXT: dealloc_stack [[TMPSTRUCT]]
  // CHECK: return [[STRVAL]]
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testAddressOnlyStructElt
func testAddressOnlyStructElt<T>(_ a : T) -> T {
  return produceAddressOnlyStruct(a).elt
  // CHECK: bb0([[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
  // CHECK: [[TMPSTRUCT:%[0-9]+]] = alloc_stack $AddressOnlyStruct<T>
  // CHECK: [[PRODFN:%[0-9]+]] = function_ref @{{.*}}produceAddressOnlyStruct
  // CHECK: apply [[PRODFN]]<T>([[TMPSTRUCT]], [[ARG1]])
  // CHECK-NEXT: [[ELTADDR:%[0-9]+]] = struct_element_addr [[TMPSTRUCT]] : $*AddressOnlyStruct<T>, #AddressOnlyStruct.elt
  // CHECK-NEXT: copy_addr [[ELTADDR]] to [init] %0 : $*T
  // CHECK-NEXT: destroy_addr [[TMPSTRUCT]]
}



// rdar://15717123 - let decls of address-only type.

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testAddressOnlyLet
func testAddressOnlyLet<T>(_ a : T) {
  let x = produceAddressOnlyStruct(a)
}


func produceSubscriptableRValue() -> [String] {}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}subscriptRValue
func subscriptRValue() {
  var a = produceSubscriptableRValue()[0]
}


struct GetOnlySubscriptStruct {
  // get-only subscript
  subscript (i : Int) -> Int { get {} }
}


// CHECK-LABEL: sil hidden [ossa] @{{.*}}testGetOnlySubscript
func testGetOnlySubscript(_ x : GetOnlySubscriptStruct, idx : Int) -> Int {
  return x[idx]
  
  // CHECK: [[SUBFN:%[0-9]+]] = function_ref @{{.*}}i
  // CHECK-NEXT: [[CALL:%[0-9]+]] = apply [[SUBFN]](
  // CHECK: return [[CALL]]
}

// Address-only let's get captured by box.
extension Optional {
  func getLV() -> Int { }
}
struct CloseOverAddressOnlyConstant<T> {
  func isError() {
    let AOV: T?
    takeClosure({ AOV.getLV() })
  }
  
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}callThroughLet
func callThroughLet(_ predicate: @escaping (Int, Int) -> Bool) {
  let p = predicate
  if p(1, 2) {
  }
}


// Verify that we can emit address-only rvalues directly into the result slot in
// chained calls.
struct GenericTestStruct<T> {
   func pass_address_only_rvalue_result(_ i: Int) -> T {
     return self[i]
   }
   subscript (i : Int) -> T {
   get {}
   set {}
   }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}pass_address_only_rvalue_result
// CHECK: bb0(%0 : $*T,
// CHECK: [[FN:%[0-9]+]] = function_ref @{{.*}}GenericTestStructV{{.*}}ig
// CHECK: apply [[FN]]<T>(%0,


struct NonMutableSubscriptable {
  subscript(x : Int) -> Int {
    get {}
    nonmutating
    set {}
  }
}

func produceNMSubscriptableRValue() -> NonMutableSubscriptable {}


// CHECK-LABEL: sil hidden [ossa] @{{.*}}test_nm_subscript_get
// CHECK: bb0(%0 : $Int):
// CHECK: [[FR1:%[0-9]+]] = function_ref @{{.*}}produceNMSubscriptableRValue
// CHECK-NEXT: [[RES:%[0-9]+]] = apply [[FR1]]()
// CHECK: [[GETFN:%[0-9]+]] = function_ref @$s9let_decls23NonMutableSubscriptableV{{[_0-9a-zA-Z]*}}ig
// CHECK-NEXT: [[RES2:%[0-9]+]] = apply [[GETFN]](%0, [[RES]])
// CHECK-NEXT: return [[RES2]]
func test_nm_subscript_get(_ a : Int) -> Int {
  return produceNMSubscriptableRValue()[a]
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}test_nm_subscript_set
// CHECK: bb0(%0 : $Int):
// CHECK: [[FR1:%[0-9]+]] = function_ref @{{.*}}produceNMSubscriptableRValue
// CHECK-NEXT: [[RES:%[0-9]+]] = apply [[FR1]]()
// CHECK: [[SETFN:%[0-9]+]] = function_ref @$s9let_decls23NonMutableSubscriptableV{{[_0-9a-zA-Z]*}}is
// CHECK-NEXT: [[RES2:%[0-9]+]] = apply [[SETFN]](%0, %0, [[RES]])
func test_nm_subscript_set(_ a : Int) {
  produceNMSubscriptableRValue()[a] = a
}

struct WeirdPropertyTest {
  // This property has a mutating getter and !mutating setter.
  var p : Int {
  mutating
  get {}
  nonmutating
  set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}test_weird_property
func test_weird_property(_ v : WeirdPropertyTest, i : Int) -> Int {
  var v = v
  // CHECK: [[VBOX:%[0-9]+]] = alloc_box ${ var WeirdPropertyTest }
  // CHECK: [[VLIFETIME:%.*]] = begin_borrow [var_decl] [[VBOX]]
  // CHECK: [[PB:%.*]] = project_box [[VLIFETIME]]
  // CHECK: store %0 to [trivial] [[PB]]

  // The setter isn't mutating, so we need to load the box.
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]]
  // CHECK: [[VVAL:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK: [[SETFN:%[0-9]+]] = function_ref @$s9let_decls17WeirdPropertyTestV1pSivs
  // CHECK: apply [[SETFN]](%1, [[VVAL]])
  v.p = i
  
  // The getter is mutating, so it takes the box address.
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[GETFN:%[0-9]+]] = function_ref @$s9let_decls17WeirdPropertyTestV1pSivg
  // CHECK-NEXT: [[RES:%[0-9]+]] = apply [[GETFN]]([[WRITE]])
  // CHECK: return [[RES]]
  return v.p
}


// CHECK-LABEL: sil hidden [ossa] @{{.*}}generic_identity
// CHECK: bb0(%0 : $*T, %1 : $*T):
// CHECK-NEXT: debug_value %1 : $*T, {{.*}} expr op_deref
// CHECK-NEXT: copy_addr %1 to [init] %0 : $*T
// CHECK-NOT: destroy_addr %1
// CHECK: } // end sil function '{{.*}}generic_identity{{.*}}'
func generic_identity<T>(_ a : T) -> T {
  // Should be a single copy_addr, with no temporary.
  return a
}

struct StaticLetMember {
  static let x = 5
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testStaticLetMember
func testStaticLetMember() -> Int {

  // CHECK: function_ref @{{.*}}StaticLetMemberV1xSi
  // CHECK: load {{.*}} : $*Int
  // CHECK-NEXT: return
  return StaticLetMember.x
}

protocol SimpleProtocol {
  func doSomethingGreat()
}

// Verify that no temporaries+copies are produced when calling non-@mutable
// methods on protocol and archetypes calls.

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testLetProtocolBases
// CHECK: bb0(%0 : $*any SimpleProtocol):
func testLetProtocolBases(_ p : SimpleProtocol) {
  // CHECK-NEXT: debug_value {{.*}} expr op_deref
  // CHECK-NEXT: open_existential_addr
  // CHECK-NEXT: witness_method
  // CHECK-NEXT: apply
  p.doSomethingGreat()

  // CHECK-NEXT: open_existential_addr
  // CHECK-NEXT: witness_method
  // CHECK-NEXT: apply
  p.doSomethingGreat()
  
  // CHECK-NOT: destroy_addr %0
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testLetArchetypeBases
// CHECK: bb0(%0 : $*T):
func testLetArchetypeBases<T : SimpleProtocol>(_ p : T) {
  // CHECK-NEXT: debug_value {{.*}} expr op_deref
  // CHECK-NEXT: witness_method $T
  // CHECK-NEXT: apply
  p.doSomethingGreat()
  // CHECK-NEXT: witness_method $T
  // CHECK-NEXT: apply
  p.doSomethingGreat()

  // CHECK-NOT: destroy_addr %0
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testDebugValue
// CHECK: bb0(%0 : $Int, %1 : $*any SimpleProtocol):
// CHECK-NEXT: debug_value %0 : $Int, let, name "a"
// CHECK-NEXT: debug_value %1 : $*any SimpleProtocol, let, name "b", {{.*}} expr op_deref
func testDebugValue(_ a : Int, b : SimpleProtocol) -> Int {

  // CHECK-NEXT: [[MV:%.*]] = move_value [var_decl] %0 : $Int
  // CHECK-NEXT: debug_value [[MV]] : $Int, let, name "x"
  let x = a

  // CHECK: apply
  b.doSomethingGreat()
  
  // CHECK-NOT: destroy_addr

  // CHECK: return [[MV]]
  return x
}


// CHECK-LABEL: sil hidden [ossa] @{{.*}}testAddressOnlyTupleArgument
func testAddressOnlyTupleArgument(_ bounds: (start: SimpleProtocol, pastEnd: Int)) {
// CHECK:       bb0(%0 : $*any SimpleProtocol, %1 : $Int):
// CHECK-NEXT:    %2 = alloc_stack [lexical] [var_decl] $(start: any SimpleProtocol, pastEnd: Int), let, name "bounds", argno 1
// CHECK-NEXT:    %3 = tuple_element_addr %2 : $*(start: any SimpleProtocol, pastEnd: Int), 0
// CHECK-NEXT:    copy_addr %0 to [init] %3 : $*any SimpleProtocol
// CHECK-NEXT:    %5 = tuple_element_addr %2 : $*(start: any SimpleProtocol, pastEnd: Int), 1
// CHECK-NEXT:    store %1 to [trivial] %5 : $*Int
// CHECK-NEXT:    destroy_addr %2 : $*(start: any SimpleProtocol, pastEnd: Int)
// CHECK-NEXT:    dealloc_stack %2 : $*(start: any SimpleProtocol, pastEnd: Int)
}


func address_only_let_closure<T>(_ x:T) -> T {
  return { { x }() }()
}

struct GenericFunctionStruct<T, U> {
  var f: (T) -> U
}


// CHECK-LABEL: sil hidden [ossa] @{{.*}}member_ref_abstraction_change
// CHECK: function_ref reabstraction thunk helper
// CHECK: return
func member_ref_abstraction_change(_ x: GenericFunctionStruct<Int, Int>) -> (Int) -> Int {
  return x.f
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}call_auto_closure
// CHECK: bb0([[CLOSURE:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> Bool):
// CHECK:   [[CLOSUREC:%.*]] = copy_value [[CLOSURE]]
// CHECK:   [[CLOSUREB:%.*]] = begin_borrow [[CLOSUREC]]
// CHECK:   apply [[CLOSUREB]]() : $@noescape @callee_guaranteed () -> Bool
// CHECK: } // end sil function '{{.*}}call_auto_closure{{.*}}'
func call_auto_closure(x: @autoclosure () -> Bool) -> Bool {
  return x()  // Calls of autoclosures should be marked transparent.
}


class SomeClass {}

struct AnotherStruct {
  var i : Int
  var c : SomeClass
}

struct StructMemberTest {
  var c : SomeClass
  var i = 42
  var s : AnotherStruct
  var t : (Int, AnotherStruct)

  // rdar://15867140 - Accessing the int member here should not copy_value the
  // whole struct.
  func testIntMemberLoad() -> Int {
    return i
  }
  // CHECK-LABEL: sil hidden [ossa] @$s9let_decls16StructMemberTestV07testIntD4LoadSiyF : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $StructMemberTest):
  // CHECK:  debug_value [[ARG]] : $StructMemberTest, let, name "self"
  // CHECK:  [[TRIVIAL_VALUE:%.*]] = struct_extract [[ARG]] : $StructMemberTest, #StructMemberTest.i
  // CHECK-NOT:  destroy_value [[ARG]] : $StructMemberTest
  // CHECK:  return [[TRIVIAL_VALUE]] : $Int

  // Accessing the int member in s should not copy_value the whole struct.
  func testRecursiveIntMemberLoad() -> Int {
    return s.i
  }
  // CHECK-LABEL: sil hidden [ossa] @$s9let_decls16StructMemberTestV016testRecursiveIntD4LoadSiyF : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $StructMemberTest):
  // CHECK:  [[EXT_1:%.*]] = struct_extract [[ARG]] : $StructMemberTest, #StructMemberTest.s
  // CHECK:  [[EXT_1_COPY:%.*]] = copy_value [[EXT_1]]
  // CHECK:  [[EXT_1_COPY_BORROW:%.*]] = begin_borrow [[EXT_1_COPY]]
  // CHECK:  [[EXT_2:%.*]] = struct_extract [[EXT_1_COPY_BORROW]] : $AnotherStruct, #AnotherStruct.i
  // CHECK:  destroy_value [[EXT_1_COPY]]
  // CHECK:  return [[EXT_2]] : $Int
  // CHECK: } // end sil function '$s9let_decls16StructMemberTestV016testRecursiveIntD4LoadSiyF'
  
  func testTupleMemberLoad() -> Int {
    return t.1.i
  }
  // CHECK-LABEL: sil hidden [ossa] @$s9let_decls16StructMemberTestV09testTupleD4LoadSiyF : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0([[ARG]] : @guaranteed $StructMemberTest):
  // CHECK:   [[T0:%.*]] = struct_extract [[ARG]] : $StructMemberTest, #StructMemberTest.t
  // CHECK:   [[T0_COPY:%.*]] = copy_value [[T0]]
  // CHECK:   ({{%.*}}, [[T2:%.*]]) = destructure_tuple [[T0_COPY]]
  // CHECK:   [[T2_BORROW:%.*]] = begin_borrow [[T2]]
  // CHECK:   [[T3:%.*]] = struct_extract [[T2_BORROW]] : $AnotherStruct, #AnotherStruct.i
  // CHECK:   return [[T3]] : $Int
  // CHECK: } // end sil function '$s9let_decls16StructMemberTestV09testTupleD4LoadSiyF'

}

struct GenericStruct<T> {
  var a : T
  var b : Int

  func getA() -> T {
    return a
  }
  // CHECK-LABEL: sil hidden [ossa] @{{.*}}GenericStructV4getA{{.*}} : $@convention(method) <T> (@in_guaranteed GenericStruct<T>) -> @out T
  // CHECK: bb0(%0 : $*T, %1 : $*GenericStruct<T>):
  // CHECK-NEXT: debug_value %1 : $*GenericStruct<T>, let, name "self", {{.*}} expr op_deref
  // CHECK-NEXT: %3 = struct_element_addr %1 : $*GenericStruct<T>, #GenericStruct.a
  // CHECK-NEXT: copy_addr %3 to [init] %0 : $*T
  // CHECK-NEXT: %5 = tuple ()
  // CHECK-NEXT: return %5 : $()

  func getB() -> Int {
    return b
  }
  
  // CHECK-LABEL: sil hidden [ossa] @{{.*}}GenericStructV4getB{{.*}} : $@convention(method) <T> (@in_guaranteed GenericStruct<T>) -> Int
  // CHECK: bb0([[SELF_ADDR:%.*]] : $*GenericStruct<T>):
  // CHECK-NEXT: debug_value [[SELF_ADDR]] : $*GenericStruct<T>, let, name "self", {{.*}} expr op_deref
  // CHECK-NEXT: [[PROJ_ADDR:%.*]] = struct_element_addr [[SELF_ADDR]] : $*GenericStruct<T>, #GenericStruct.b
  // CHECK-NEXT: [[PROJ_VAL:%.*]] = load [trivial] [[PROJ_ADDR]] : $*Int
  // CHECK-NEXT: return [[PROJ_VAL]] : $Int
}


// rdar://15877337
struct LetPropertyStruct {
  let lp : Int
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testLetPropertyAccessOnLValueBase
// CHECK: bb0(%0 : $LetPropertyStruct):
// CHECK:  [[ABOX:%[0-9]+]] = alloc_box ${ var LetPropertyStruct }
// CHECK:  [[ALIFETIME:%[0-9]+]] = begin_borrow [var_decl] [[ABOX]]
// CHECK:  [[A:%[0-9]+]] = project_box [[ALIFETIME]]
// CHECK:   store %0 to [trivial] [[A]] : $*LetPropertyStruct
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[A]]
// CHECK:   [[STRUCT:%[0-9]+]] = load [trivial] [[READ]] : $*LetPropertyStruct
// CHECK:   [[PROP:%[0-9]+]] = struct_extract [[STRUCT]] : $LetPropertyStruct, #LetPropertyStruct.lp
// CHECK:   destroy_value [[ABOX]] : ${ var LetPropertyStruct }
// CHECK:   return [[PROP]] : $Int
func testLetPropertyAccessOnLValueBase(_ a : LetPropertyStruct) -> Int {
  var a = a
  return a.lp
}


var addressOnlyGetOnlyGlobalProperty : SimpleProtocol { get {} }

// CHECK-LABEL: sil hidden [ossa] @$s9let_decls018testAddressOnlyGetE14GlobalPropertyAA14SimpleProtocol_pyF
// CHECK: bb0(%0 : $*any SimpleProtocol):
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:  %1 = function_ref @$s9let_decls014addressOnlyGetD14GlobalPropertyAA14SimpleProtocol_pvg
// CHECK-NEXT:  %2 = apply %1(%0) : $@convention(thin) () -> @out any SimpleProtocol
// CHECK-NEXT:  %3 = tuple ()
// CHECK-NEXT:  return %3 : $()
// CHECK-NEXT: }
func testAddressOnlyGetOnlyGlobalProperty() -> SimpleProtocol {
  return addressOnlyGetOnlyGlobalProperty
}


// rdar://15962740
struct LetDeclInStruct {
    let immutable: Int
    init() {
        immutable = 1
    }
}

// rdar://19854166 - Swift 1.2 uninitialized constant causes crash
// The destroy_addr for a let stack temporary should be generated against 
// mark_uninitialized instruction, so DI will see it.
func test_unassigned_let_constant() {
  let string : String
}
// CHECK: [[S:%[0-9]+]] = alloc_stack [var_decl] $String, let, name "string"
// CHECK-NEXT:  [[MUI:%[0-9]+]] = mark_uninitialized [var] [[S]] : $*String
// CHECK-NEXT:  destroy_addr [[MUI]] : $*String
// CHECK-NEXT:  dealloc_stack [[S]] : $*String

