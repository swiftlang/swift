// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func takeClosure(a : () -> Int) {}

// Let decls don't get boxes for trivial types.
//
// CHECK-LABEL: sil hidden @{{.*}}test1
func test1(let a : Int) -> Int {
  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_stack

  let (b,c) = (a, 32)

  return b+c
  
  // CHECK: return
}

// rdar://15716277
// CHECK: @{{.*}}let_destructuring
func let_destructuring() -> Int {
  let (a, b) = ((1,2), 5)
  return a.1+a.0+b
}

// Let decls being closed over.
//
// CHECK-LABEL: sil hidden @{{.*}}test2
func test2() {
  // No allocations.
  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_stack

  let x = 42

  takeClosure({x})


  // CHECK: return
}

// The closure just returns its value, which it captured directly.

// CHECK: sil shared @_TFF9let_decls5test2FT_T_U_FT_Si : $@convention(thin) (Int) -> Int
// CHECK: bb0(%0 : $Int):
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
func useAString(a : String) {}

// rdar://15689514 - Verify that the cleanup for the let decl runs at the end of
// the 'let' lifetime, not at the end of the initializing expression.
//
// CHECK-LABEL: sil hidden @{{.*}}test3
func test3() {
  // CHECK: [[GETFN:%[0-9]+]] = function_ref{{.*}}getAString
  // CHECK-NEXT: [[STR:%[0-9]+]] = apply [[GETFN]]()
  let o = getAString()
  
  // CHECK-NOT: release_value

  // CHECK: [[USEFN:%[0-9]+]] = function_ref{{.*}}useAString
  // CHECK-NEXT: retain_value [[STR]]
  // CHECK-NEXT: [[USE:%[0-9]+]] = apply [[USEFN]]([[STR]])
  useAString(o)
  
  // CHECK: release_value [[STR]]
}



struct AddressOnlyStruct<T> {
  var elt : T
  var str : String
}

func produceAddressOnlyStruct<T>(x : T) -> AddressOnlyStruct<T> {}

// CHECK-LABEL: sil hidden @{{.*}}testAddressOnlyStructString
func testAddressOnlyStructString<T>(a : T) -> String {
  return produceAddressOnlyStruct(a).str
  
  // CHECK: [[PRODFN:%[0-9]+]] = function_ref @{{.*}}produceAddressOnlyStruct
  // CHECK: [[TMPSTRUCT:%[0-9]+]] = alloc_stack $AddressOnlyStruct<T>
  // CHECK: apply [[PRODFN]]<T>([[TMPSTRUCT]]#1,
  // CHECK-NEXT: [[STRADDR:%[0-9]+]] = struct_element_addr [[TMPSTRUCT]]#1 : $*AddressOnlyStruct<T>, #AddressOnlyStruct.str
  // CHECK-NEXT: [[STRVAL:%[0-9]+]] = load [[STRADDR]]
  // CHECK-NEXT: retain_value [[STRVAL]]
  // CHECK-NEXT: destroy_addr [[TMPSTRUCT]]
  // CHECK-NEXT: dealloc_stack [[TMPSTRUCT]]
  // CHECK: return [[STRVAL]]
}

// CHECK-LABEL: sil hidden @{{.*}}testAddressOnlyStructElt
func testAddressOnlyStructElt<T>(a : T) -> T {
  return produceAddressOnlyStruct(a).elt
  
  // CHECK: [[PRODFN:%[0-9]+]] = function_ref @{{.*}}produceAddressOnlyStruct
  // CHECK: [[TMPSTRUCT:%[0-9]+]] = alloc_stack $AddressOnlyStruct<T>
  // CHECK: apply [[PRODFN]]<T>([[TMPSTRUCT]]#1,
  // CHECK-NEXT: [[ELTADDR:%[0-9]+]] = struct_element_addr [[TMPSTRUCT]]#1 : $*AddressOnlyStruct<T>, #AddressOnlyStruct.elt
  // CHECK-NEXT: copy_addr [[ELTADDR]] to [initialization] %0 : $*T
  // CHECK-NEXT: destroy_addr [[TMPSTRUCT]]
}



// rdar://15717123 - let decls of address-only type.

// CHECK-LABEL: sil hidden @{{.*}}testAddressOnlyLet
func testAddressOnlyLet<T>(a : T) {
  let x = produceAddressOnlyStruct(a)
}


func produceSubscriptableRValue() -> [String] {}

// CHECK-LABEL: sil hidden @{{.*}}subscriptRValue
func subscriptRValue() {
  var a = produceSubscriptableRValue()[0]
}


struct GetOnlySubscriptStruct {
  // get-only subscript
  subscript (i : Int) -> Int { get {} }
}


// CHECK-LABEL: sil hidden @{{.*}}testGetOnlySubscript
func testGetOnlySubscript(x : GetOnlySubscriptStruct, idx : Int) -> Int {
  return x[idx]
  
  // CHECK: [[SUBFN:%[0-9]+]] = function_ref @{{.*}}g9subscript
  // CHECK-NEXT: [[CALL:%[0-9]+]] = apply [[SUBFN]](
  // CHECK: return [[CALL]]
}

// Address-only let's get captured by box.
extension Optional {
  func getLV() -> Int { }
}
struct CloseOverAddressOnlyConstant<T>  {
  func isError() {
    let AOV = Optional<T>()
    takeClosure({ AOV.getLV() })
  }
  
}

// CHECK-LABEL: sil hidden @{{.*}}callThroughLet
func callThroughLet(predicate: (Int, Int) -> Bool) {
  let p = predicate
  if p(1, 2) {
  }
}


// Verify that the mangling of a symbol doesn't depent on whether an argument
// is var or let.
func var_argument_mangling(var a : Int) {}
func let_argument_mangling(let a : Int) {}
func def_argument_mangling(a : Int) {}
// CHECK: sil hidden @[[MANPREFIX:[^ :]*]]var_argument_mangling[[MANSUFFIX:[^ :]*]] : $
// CHECK: sil hidden @[[MANPREFIX]]let_argument_mangling[[MANSUFFIX]] : $
// CHECK: sil hidden @[[MANPREFIX]]def_argument_mangling[[MANSUFFIX]] : $



// Verify that we can emit address-only rvalues directly into the result slot in
// chained calls.
struct GenericTestStruct<T> {
   func pass_address_only_rvalue_result(i: Int) -> T {
     return self[i]
   }
   subscript (i : Int) -> T {
   get {}
   set {}
   }
}

// CHECK-LABEL: sil hidden @{{.*}}pass_address_only_rvalue_result
// CHECK: bb0(%0 : $*T,
// CHECK: [[FN:%[0-9]+]] = function_ref @{{.*}}GenericTestStructg9subscript
// CHECK: apply [[FN]]<T>(%0,


struct NonMutableSubscriptable {
  subscript(x : Int) -> Int {
    get {}
    nonmutating
    set {}
  }
}

func produceNMSubscriptableRValue() -> NonMutableSubscriptable {}


// CHECK-LABEL: sil hidden @{{.*}}test_nm_subscript_get
// CHECK: bb0(%0 : $Int):
// CHECK: [[FR1:%[0-9]+]] = function_ref @{{.*}}produceNMSubscriptableRValue
// CHECK-NEXT: [[RES:%[0-9]+]] = apply [[FR1]]()
// CHECK: [[GETFN:%[0-9]+]] = function_ref @_TFV9let_decls23NonMutableSubscriptableg9subscript
// CHECK-NEXT: [[RES2:%[0-9]+]] = apply [[GETFN]](%0, [[RES]])
// CHECK-NEXT: return [[RES2]]
func test_nm_subscript_get(let a : Int) -> Int {
  return produceNMSubscriptableRValue()[a]
}

// CHECK-LABEL: sil hidden @{{.*}}test_nm_subscript_set
// CHECK: bb0(%0 : $Int):
// CHECK: [[FR1:%[0-9]+]] = function_ref @{{.*}}produceNMSubscriptableRValue
// CHECK-NEXT: [[RES:%[0-9]+]] = apply [[FR1]]()
// CHECK: [[SETFN:%[0-9]+]] = function_ref @_TFV9let_decls23NonMutableSubscriptables9subscript
// CHECK-NEXT: [[RES2:%[0-9]+]] = apply [[SETFN]](%0, %0, [[RES]])
func test_nm_subscript_set(let a : Int) {
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

// CHECK-LABEL: sil hidden @{{.*}}test_weird_property
func test_weird_property(var v : WeirdPropertyTest, let i : Int) -> Int {
  // CHECK: [[VBOX:%[0-9]+]] = alloc_box $WeirdPropertyTest
  // CHECK: store %0 to [[VBOX]]#1

  // The setter isn't mutating, so we need to load the box.
  // CHECK: [[VVAL:%[0-9]+]] = load [[VBOX]]#1
  // CHECK: [[SETFN:%[0-9]+]] = function_ref @_TFV9let_decls17WeirdPropertyTests1pSi
  // CHECK: apply [[SETFN]](%1, [[VVAL]])
  v.p = i
  
  // The getter is mutating, so it takes the box address.
  // CHECK: [[GETFN:%[0-9]+]] = function_ref @_TFV9let_decls17WeirdPropertyTestg1pSi
  // CHECK-NEXT: [[RES:%[0-9]+]] = apply [[GETFN]]([[VBOX]]#1)
  // CHECK: return [[RES]]
  return v.p
}


// CHECK-LABEL: sil hidden @{{.*}}generic_identity
// CHECK-NEXT: bb0(%0 : $*T, %1 : $*T):
// CHECK-NEXT: debug_value_addr %1 : $*T
// CHECK-NEXT: copy_addr [take] %1 to [initialization] %0 : $*T
// CHECK-NEXT: %4 = tuple ()
// CHECK-NEXT: return %4
func generic_identity<T>(let a : T) -> T {
  // Should be a single copy_addr, with no temporary.
  return a
}

struct StaticLetMember {
  static let x = 5
}

// CHECK-LABEL: sil hidden @{{.*}}testStaticLetMember
func testStaticLetMember() -> Int {

  // CHECK: function_ref @{{.*}}StaticLetMemberau1xSi
  // CHECK: load {{.*}} : $*Int
  // CHECK-NEXT: return
  return StaticLetMember.x
}

protocol SimpleProtocol {
  func doSomethingGreat()
}

// Verify that no temporaries+copies are produced when calling non-@mutable
// methods on protocol and archetypes calls.

// CHECK-LABEL: sil hidden @{{.*}}testLetProtocolBases
// CHECK-NEXT: bb0(%0 : $*SimpleProtocol):
func testLetProtocolBases(let p : SimpleProtocol) {
  // CHECK-NEXT: debug_value_addr
  // CHECK-NEXT: open_existential_addr
  // CHECK-NEXT: witness_method
  // CHECK-NEXT: apply
  p.doSomethingGreat()

  // CHECK-NEXT: open_existential_addr
  // CHECK-NEXT: witness_method
  // CHECK-NEXT: apply
  p.doSomethingGreat()
  
  // CHECK-NEXT: destroy_addr %0
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @{{.*}}testLetArchetypeBases
// CHECK-NEXT: bb0(%0 : $*T):
func testLetArchetypeBases<T : SimpleProtocol>(let p : T) {
  // CHECK-NEXT: debug_value_addr
  // CHECK-NEXT: witness_method $T
  // CHECK-NEXT: apply
  p.doSomethingGreat()
  // CHECK-NEXT: witness_method $T
  // CHECK-NEXT: apply
  p.doSomethingGreat()

  // CHECK-NEXT: destroy_addr %0
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @{{.*}}testDebugValue
// CHECK-NEXT: bb0(%0 : $Int, %1 : $*SimpleProtocol):
// CHECK-NEXT: debug_value %0 : $Int  // let a
// CHECK-NEXT: debug_value_addr %1 : $*SimpleProtocol  // let b
func testDebugValue(let a : Int, let b : SimpleProtocol) -> Int {

  // CHECK-NEXT: debug_value %0 : $Int  // let x
  let x = a

  // CHECK: apply
  b.doSomethingGreat()
  
  // CHECK: destroy_addr

  // CHECK: return %0
  return x
}


// CHECK-LABEL: sil hidden @{{.*}}testAddressOnlyTupleArgument
func testAddressOnlyTupleArgument(let bounds: (start: SimpleProtocol, pastEnd: Int)) {
// CHECK-NEXT:  bb0(%0 : $*SimpleProtocol, %1 : $Int):
// CHECK-NEXT:    %2 = alloc_stack $(start: SimpleProtocol, pastEnd: Int)  // let bounds
// CHECK-NEXT:    %3 = tuple_element_addr %2#1 : $*(start: SimpleProtocol, pastEnd: Int), 0
// CHECK-NEXT:    copy_addr [take] %0 to [initialization] %3 : $*SimpleProtocol
// CHECK-NEXT:    %5 = tuple_element_addr %2#1 : $*(start: SimpleProtocol, pastEnd: Int), 1
// CHECK-NEXT:    store %1 to %5 : $*Int
// CHECK-NEXT:    debug_value_addr %2
// CHECK-NEXT:    destroy_addr %2#1 : $*(start: SimpleProtocol, pastEnd: Int)
// CHECK-NEXT:    dealloc_stack %2#0 : $*@local_storage (start: SimpleProtocol, pastEnd: Int)
}


func address_only_let_closure<T>(let x:T) -> T {
  return { { x }() }()
}

struct GenericFunctionStruct<T, U> {
  var f: T -> U
}


// CHECK-LABEL: sil hidden @{{.*}}member_ref_abstraction_change
// CHECK: function_ref reabstraction thunk helper
// CHECK: return
func member_ref_abstraction_change(let x: GenericFunctionStruct<Int, Int>) -> Int -> Int {
  return x.f
}

// CHECK-LABEL: sil hidden @{{.*}}call_auto_closure
// CHECK: apply %0()
func call_auto_closure(@autoclosure let x: () -> Bool) -> Bool {
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

  // rdar://15867140 - Accessing the int member here should not retain the
  // whole struct.
  func testIntMemberLoad() -> Int {
    return i
  }
  // CHECK-LABEL: sil hidden @{{.*}}testIntMemberLoad{{.*}} : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0(%0 : $StructMemberTest):
  // CHECK:  debug_value %0 : $StructMemberTest  // let self
  // CHECK:  %2 = struct_extract %0 : $StructMemberTest, #StructMemberTest.i
  // CHECK-NOT:  release_value %0 : $StructMemberTest
  // CHECK:  return %2 : $Int

  // Accessing the int member in s should not retain the whole struct.
  func testRecursiveIntMemberLoad() -> Int {
    return s.i
  }
  // CHECK-LABEL: sil hidden @{{.*}}testRecursiveIntMemberLoad{{.*}} : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0(%0 : $StructMemberTest):
  // CHECK:  debug_value %0 : $StructMemberTest  // let self
  // CHECK:  %2 = struct_extract %0 : $StructMemberTest, #StructMemberTest.s
  // CHECK:  %3 = struct_extract %2 : $AnotherStruct, #AnotherStruct.i
  // CHECK-NOT:  release_value %0 : $StructMemberTest
  // CHECK:  return %3 : $Int
  
  func testTupleMemberLoad() -> Int {
    return t.1.i
  }
  // CHECK-LABEL: sil hidden @{{.*}}testTupleMemberLoad{{.*}} : $@convention(method) (@guaranteed StructMemberTest)
  // CHECK: bb0(%0 : $StructMemberTest):
  // CHECK-NEXT:   debug_value %0 : $StructMemberTest  // let self
  // CHECK-NEXT:   [[T0:%.*]] = struct_extract %0 : $StructMemberTest, #StructMemberTest.t
  // CHECK-NEXT:   [[T1:%.*]] = tuple_extract [[T0]] : $(Int, AnotherStruct), 0
  // CHECK-NEXT:   [[T2:%.*]] = tuple_extract [[T0]] : $(Int, AnotherStruct), 1
  // CHECK-NEXT:   [[T3:%.*]] = struct_extract [[T2]] : $AnotherStruct, #AnotherStruct.i
  // CHECK-NEXT:   return [[T3]] : $Int

}

struct GenericStruct<T> {
  var a : T
  var b : Int

  func getA() -> T {
    return a
  }
  // CHECK-LABEL: sil hidden @{{.*}}GenericStruct4getA{{.*}} : $@convention(method) <T> (@out T, @in_guaranteed GenericStruct<T>)
  // CHECK-NEXT: bb0(%0 : $*T, %1 : $*GenericStruct<T>):
  // CHECK-NEXT: debug_value_addr %1 : $*GenericStruct<T>  // let self
  // CHECK-NEXT: %3 = struct_element_addr %1 : $*GenericStruct<T>, #GenericStruct.a
  // CHECK-NEXT: copy_addr %3 to [initialization] %0 : $*T
  // CHECK-NEXT: %5 = tuple ()
  // CHECK-NEXT: return %5 : $()

  func getB() -> Int {
    return b
  }
  
  // CHECK-LABEL: sil hidden @{{.*}}GenericStruct4getB{{.*}} : $@convention(method) <T> (@in_guaranteed GenericStruct<T>) -> Int
  // CHECK-NEXT: bb0(%0 : $*GenericStruct<T>):
  // CHECK-NEXT: debug_value_addr %0 : $*GenericStruct<T>  // let self
  // CHECK-NEXT: %2 = struct_element_addr %0 : $*GenericStruct<T>, #GenericStruct.b
  // CHECK-NEXT: %3 = load %2 : $*Int
  // CHECK-NOT: destroy_addr %0 : $*GenericStruct<T>
  // CHECK-NEXT: return %3 : $Int
}


// rdar://15877337
struct LetPropertyStruct {
  let lp : Int
}

// CHECK-LABEL: sil hidden @{{.*}}testLetPropertyAccessOnLValueBase
// CHECK: bb0(%0 : $LetPropertyStruct):
// CHECK:  %1 = alloc_box $LetPropertyStruct
// CHECK:   store %0 to %1#1 : $*LetPropertyStruct
// CHECK:   %3 = load %1#1 : $*LetPropertyStruct
// CHECK:   %4 = struct_extract %3 : $LetPropertyStruct, #LetPropertyStruct.lp
// CHECK:   strong_release %1#0 : $Builtin.NativeObject
// CHECK:   return %4 : $Int
func testLetPropertyAccessOnLValueBase(var a : LetPropertyStruct) -> Int {
  return a.lp
}


var addressOnlyGetOnlyGlobalProperty : SimpleProtocol { get {} }

// CHECK-LABEL: sil hidden @{{.*}}testAddressOnlyGetOnlyGlobalProperty
// CHECK-NEXT: bb0(%0 : $*SimpleProtocol):
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:  %1 = function_ref @{{.*}}addressOnlyGetOnlyGlobalProperty
// CHECK-NEXT:  %2 = apply %1(%0) : $@convention(thin) (@out SimpleProtocol) -> ()
// CHECK-NEXT:  %3 = tuple ()
// CHECK-NEXT:  return %3 : $()
// CHECK-NEXT: }
func testAddressOnlyGetOnlyGlobalProperty() -> SimpleProtocol {
  return addressOnlyGetOnlyGlobalProperty
}


// rdar://15933538 let decls with StoredObjC storage type should have a getter
// synthesized, but not a setter.
@objc
class C  {
  let x : Int = 100
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
// CHECK: [[S:%[0-9]+]] = alloc_stack $String  // let string
// CHECK-NEXT:  [[MUI:%[0-9]+]] = mark_uninitialized [var] [[S]]#1 : $*String
// CHECK-NEXT:  destroy_addr [[MUI]] : $*String
// CHECK-NEXT:  dealloc_stack [[S]]#0 : $*@local_storage String

