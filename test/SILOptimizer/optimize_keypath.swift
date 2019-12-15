// RUN: %empty-directory(%t) 
// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -emit-sil >%t/output.sil
// RUN: %FileCheck %s < %t/output.sil
// RUN: %FileCheck -check-prefix=CHECK-ALL %s < %t/output.sil

// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=x86_64

protocol P {
  func modifyIt()
  var computed: Int { get set }
  
  subscript(a: Int, b: Bool, c: Int?) -> Int { get set }
}

struct GenStruct<T : P> : P {
  var st: T
  var computed: Int { get { st.computed } set { st.computed = newValue } }
  
  subscript(a: Int, b: Bool, c: Int?) -> Int { get { st[a, b, c] } set { st[a, b, c] = newValue } }
  
  init(_ st: T) { self.st = st }

  func modifyIt() {
    st.modifyIt()
  }
}

var numGenClassObjs = 0

final class GenClass<T : P> : P {
  var ct: T
  var computed: Int { get { ct.computed } set { ct.computed = newValue } }
  
  subscript(a: Int, b: Bool, c: Int?) -> Int { get { ct[a, b, c] } set { ct[a, b, c] = newValue } }

  init(_ ct: T) {
    self.ct = ct
    numGenClassObjs += 1
  }

  deinit {
    numGenClassObjs -= 1
  }

  func modifyIt() {
    ct.modifyIt()
  }
}

class Base<T> {
  final var i: Int = 12
}

class DerivedClass<T> : Base<T> {
}

final class DerivedClass2 : DerivedClass<Int> {
}

final class SimpleClass : P {
  var i: Int
  var array = [1, 2, 3, 4, 5]
  static var numObjs = 0

  init(_ i: Int) {
    self.i = i
    Self.numObjs += 1
  }

  deinit {
    Self.numObjs -= 1
  }

  func modifyIt() {
    i += 10
  }
  
  var computed: Int { get { i + 1 } set { i = newValue - 1} }
  
  subscript(a: Int, b: Bool, c: Int?) -> Int {
    get { precondition(!b && c == nil, "arguments are corrupt"); return array[a] }
    set { precondition(b && c == 42, "arguments are corrupt"); array[a] = newValue }
  }
}

struct SimpleStruct {
  var tuple = (0, 1)
  
  struct Nested {
    var i: Int
  }
  var opt: Nested?
  
  var i = 0
  
  var computed: Int { get { i + 1 } set { i = newValue - 1} }
}

// Check if all keypath instructions have been optimized away
// CHECK-ALL-NOT: = keypath

// CHECK-LABEL: sil {{.*}}testGenStructRead
// CHECK: [[A:%[0-9]+]] = struct_element_addr %1
// CHECK: copy_addr [[A]] to [initialization] %0
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenStructRead<T>(_ s: GenStruct<T>) -> T {
  let kp = \GenStruct<T>.st
  return s[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testGenStructWrite
// CHECK: [[A:%[0-9]+]] = struct_element_addr %0
// CHECK: copy_addr %1 to [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenStructWrite<T>(_ s: inout GenStruct<T>, _ t: T) {
  let kp = \GenStruct<T>.st
  s[keyPath: kp] = t
}

// CHECK-LABEL: sil {{.*}}testGenClassRead
// CHECK: [[E:%[0-9]+]] = ref_element_addr %1
// CHECK: [[A:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E]]
// CHECK: copy_addr [[A]] to [initialization] %0
// CHECK: end_access [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenClassRead<T>(_ c: GenClass<T>) -> T {
  let kp = \GenClass<T>.ct
  return c[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testDerivedClassRead
// CHECK: [[C:%[0-9]+]] = upcast %0
// CHECK: [[E:%[0-9]+]] = ref_element_addr [[C]]
// CHECK: [[A:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E]]
// CHECK: [[V:%[0-9]+]] = load [[A]]
// CHECK: end_access [[A]]
// CHECK: return [[V]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testDerivedClassRead<T>(_ c: DerivedClass<T>) -> Int {
  let kp = \DerivedClass<T>.i
  return c[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testDerivedClass2Read
// CHECK: [[C:%[0-9]+]] = upcast %0
// CHECK: [[E:%[0-9]+]] = ref_element_addr [[C]]
// CHECK: [[A:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E]]
// CHECK: [[V:%[0-9]+]] = load [[A]]
// CHECK: end_access [[A]]
// CHECK: return [[V]]
@inline(never)
func testDerivedClass2Read(_ c: DerivedClass2) -> Int {
  let kp = \DerivedClass2.i
  return c[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testGenClassWrite
// CHECK: [[S:%[0-9]+]] = alloc_stack $T
// CHECK: [[E:%[0-9]+]] = ref_element_addr %0
// CHECK: [[A:%[0-9]+]] = begin_access [modify] [dynamic] [[E]]
// CHECK: copy_addr [take] [[S]] to [[A]]
// CHECK: end_access [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenClassWrite<T>(_ c: GenClass<T>, _ t: T) {
  let kp = \GenClass<T>.ct
  c[keyPath: kp] = t
}


@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func modifyGeneric<T : P>(_ t: inout T) {
  t.modifyIt()
}

// CHECK-LABEL: sil {{.*}}testGenStructModify
// CHECK: [[A:%[0-9]+]] = struct_element_addr %0
// CHECK: [[F:%[0-9]+]] = function_ref {{.*}}modifyGeneric
// CHECK: apply [[F]]<T>([[A]])
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenStructModify<T : P>(_ s: inout GenStruct<T>) {
  let kp = \GenStruct<T>.st
  modifyGeneric(&s[keyPath: kp])
}

// CHECK-LABEL: sil {{.*}}testGenClassModify
// CHECK: [[E:%[0-9]+]] = ref_element_addr %0
// CHECK: [[A:%[0-9]+]] = begin_access [modify] [dynamic] [[E]]
// CHECK: [[F:%[0-9]+]] = function_ref {{.*}}modifyGeneric
// CHECK: apply [[F]]<T>([[A]])
// CHECK: end_access [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGenClassModify<T : P>(_ c: GenClass<T>) {
  let kp = \GenClass<T>.ct
  modifyGeneric(&c[keyPath: kp])
}

// CHECK-LABEL: sil {{.*}}testNestedRead1
// CHECK: [[R1:%[0-9]+]] = struct_extract %0
// CHECK: [[E1:%[0-9]+]] = ref_element_addr [[R1]]
// CHECK: [[A1:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E1]]
// CHECK: [[E2:%[0-9]+]] = struct_element_addr [[A1]]
// CHECK: [[R2:%[0-9]+]] = load [[E2]]
// CHECK: end_access [[A1]]
// CHECK: [[E3:%[0-9]+]] = ref_element_addr [[R2]]
// CHECK: [[A2:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E3]]
// CHECK: [[I:%[0-9]+]] = load [[A2]]
// CHECK: end_access [[A2]]
// CHECK: return [[I]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testNestedRead1(_ s: GenStruct<GenClass<GenStruct<SimpleClass>>>) -> Int {
  let kp = \GenStruct<GenClass<GenStruct<SimpleClass>>>.st.ct.st.i
  return s[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testNestedRead2
// CHECK: [[R:%[0-9]+]] = struct_extract %1
// CHECK: [[E1:%[0-9]+]] = ref_element_addr [[R]]
// CHECK: [[A:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E1]]
// CHECK: [[E2:%[0-9]+]] = struct_element_addr [[A]]
// CHECK: copy_addr [[E2]] to [initialization] %0
// CHECK: end_access [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testNestedRead2<T>(_ s: GenStruct<GenClass<GenStruct<T>>>) -> T {
  let kp = \GenStruct<GenClass<GenStruct<T>>>.st.ct.st
  return s[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testNestedWrite
// CHECK: [[E1:%[0-9]+]] = ref_element_addr %0
// CHECK: [[A1:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E1]]
// CHECK: [[E2:%[0-9]+]] = struct_element_addr [[A1]]
// CHECK: [[R1:%[0-9]+]] = load [[E2]]
// CHECK: end_access [[A1]]
// CHECK: [[E3:%[0-9]+]] = ref_element_addr [[R1]]
// CHECK: [[A2:%[0-9]+]] = begin_access [modify] [dynamic] [no_nested_conflict] [[E3]]
// CHECK: store %1 to [[A2]]
// CHECK: end_access [[A2]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testNestedWrite(_ s: GenClass<GenStruct<SimpleClass>>, _ i: Int) {
  let kp = \GenClass<GenStruct<SimpleClass>>.ct.st.i
  s[keyPath: kp] = i
}

// CHECK-LABEL: sil {{.*}}testNestedModify
// CHECK: [[E1:%[0-9]+]] = struct_element_addr %0
// CHECK: [[R1:%[0-9]+]] = load [[E1]]
// CHECK: [[E2:%[0-9]+]] = ref_element_addr [[R1]]
// CHECK: [[A1:%[0-9]+]] = begin_access [read] [dynamic] [no_nested_conflict] [[E2]]
// CHECK: [[R2:%[0-9]+]] = load [[A1]]
// CHECK: end_access [[A1]]
// CHECK: [[E3:%[0-9]+]] = ref_element_addr [[R2]]
// CHECK: [[A2:%[0-9]+]] = begin_access [modify] [dynamic] [[E3]]
// CHECK: [[F:%[0-9]+]] = function_ref {{.*}}modifyGeneric
// CHECK: apply [[F]]<T>([[A2]])
// CHECK: end_access [[A2]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testNestedModify<T : P>(_ s: inout GenStruct<GenClass<GenClass<T>>>) {
  let kp = \GenStruct<GenClass<GenClass<T>>>.st.ct.ct
  modifyGeneric(&s[keyPath: kp])
}

// CHECK-LABEL: sil {{.*}}testTuple
// CHECK: [[E:%[0-9]+]] = struct_element_addr
// CHECK: [[T1:%[0-9]+]] = tuple_element_addr [[E]]
// CHECK: [[I:%[0-9]+]] = load [[T1]]
// CHECK: [[T2:%[0-9]+]] = tuple_element_addr [[E]]
// CHECK: store [[I]] to [[T2]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testTuple(_ s: inout SimpleStruct) {
  let first = \SimpleStruct.tuple.0
  let second = \SimpleStruct.tuple.1
  s[keyPath: first] = s[keyPath: second]
}

// CHECK-LABEL: sil {{.*}} [noinline] {{.*}}testGetter
// CHECK: [[A:%[0-9]+]] = alloc_stack $Int
// CHECK: [[F:%[0-9]+]] = function_ref {{.*}}computed
// CHECK: apply [[F]]<T>([[A]], %0)
// destroy_addr gets optimized out
// CHECK: dealloc_stack [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGetter<T : P>(_ s: GenStruct<T>) -> Int {
  let kp = \GenStruct<T>.computed
  return s[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testComputedModify
// CHECK: [[A:%[0-9]+]] = alloc_stack $Int
// CHECK: [[G:%[0-9]+]] = function_ref {{.*}}computed
// CHECK: apply [[G]]<T>([[A]], %0)
// CHECK: store {{%[0-9]+}} to [[A]]
// CHECK: [[S:%[0-9]+]] = function_ref {{.*}}computed
// CHECK: apply [[S]]<T>([[A]], %0)
// destroy_addr gets optimized out
// CHECK: dealloc_stack [[A]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testComputedModify<T : P>(_ s: inout GenStruct<T>) {
  let kp = \GenStruct<T>.computed
  s[keyPath: kp] += 10
}

// CHECK-LABEL: sil {{.*}}testSubscript
// CHECK: [[I1:%[0-9]+]] = alloc_stack $Int
// CHECK: [[C1:%[0-9]+]] = alloc_stack $(Int, Bool, Optional<Int>)
// CHECK: [[A1:%[0-9]+]] = address_to_pointer [[C1]]
// CHECK: [[R1:%[0-9]+]] = struct $UnsafeRawPointer ([[A1]]
// CHECK: [[G:%[0-9]+]] = function_ref
// CHECK: apply [[G]]<T>([[I1]], {{%[0-9]+}}, [[R1]])
// CHECK: dealloc_stack [[C1]]
//
// CHECK: [[I2:%[0-9]+]] = alloc_stack $Int
// CHECK: [[C2:%[0-9]+]] = alloc_stack $(Int, Bool, Optional<Int>)
// CHECK: [[A2:%[0-9]+]] = address_to_pointer [[C2]]
// CHECK: [[R2:%[0-9]+]] = struct $UnsafeRawPointer ([[A2]]
// CHECK: [[S:%[0-9]+]] = function_ref
// CHECK: apply [[S]]<T>([[I2]], {{%[0-9]+}}, [[R2]])
// CHECK: dealloc_stack [[C2]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testSubscript<T : P>(_ s: inout GenStruct<T>) {
  let kp1 = \GenStruct<T>[1, true, 42]
  let kp2 = \GenStruct<T>[2, false, nil]
  s[keyPath: kp1] = s[keyPath: kp2]
}

// CHECK-LABEL: sil {{.*}}testModifyOptionalForce
// CHECK: [[F:%[0-9]+]] = select_enum [[O:%[0-9]+]]
// CHECK: cond_fail [[F]]
// CHECK: unchecked_enum_data [[O]]
// CHECK: [[E2:%[0-9]+]] = init_enum_data_addr [[E1:%[0-9]+]]
// CHECK: store {{%[0-9]+}} to [[E2]]
// CHECK: inject_enum_addr [[E1]]
// CHECK: return
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testModifyOptionalForce(_ s: inout SimpleStruct) {
  let kp = \SimpleStruct.opt!.i
  s[keyPath: kp] += 10
}



// CHECK-LABEL: sil {{.*}}testOptionalChain
// By the time the test gets run, lots of stack
// allocations have been promoted to registers.
//
//     Check if value is null
// CHECK: switch_enum [[O:%[0-9]+]]
// CHECK: {{bb.}}:
//         Unwrap value
//     CHECK: [[A1:%[0-9]+]] = alloc_stack
//     CHECK: store [[O]] to [[A1]]
//     CHECK: [[U:%[0-9]+]] = unchecked_take_enum_data_addr [[A1]]
//         Access stored property & re-wrap result
//     CHECK: [[I:%[0-9]+]] = struct_element_addr [[U]]
//     CHECK: [[R1:%[0-9]+]] = enum
//     CHECK: dealloc_stack [[A1]]
//     CHECK: br [[CONTINUATION:bb.]]([[R1]] : $Optional<Int>)
// CHECK: {{bb.}}:
//         Store nil in result
//     CHECK: [[R2:%[0-9]+]] = enum
//     CHECK: br [[CONTINUATION]]([[R2]] : $Optional<Int>)
// CHECK: [[CONTINUATION]]([[R:%[0-9]+]] : $Optional<Int>):
// CHECK: return [[R]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testOptionalChain(_ s: SimpleStruct) -> Int? {
  let kp = \SimpleStruct.opt?.i
  return s[keyPath: kp]
}


// CHECK-LABEL: sil {{.*}}testGetOptionalForce
// CHECK: [[F:%[0-9]+]] = select_enum [[O:%[0-9]+]]
// CHECK: cond_fail [[F]]
// CHECK: [[A:%[0-9]+]] = alloc_stack
// CHECK: store [[O]] to [[A]]
// CHECK: [[E2:%[0-9]+]] = unchecked_take_enum_data_addr [[A]]
// CHECK: [[E3:%[0-9]+]] = struct_element_addr [[E2]]
// CHECK: [[I:%[0-9]+]] = load [[E3]]
// CHECK: dealloc_stack [[A]]
// CHECK: return [[I]]
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGetOptionalForce(_ s: SimpleStruct) -> Int {
  let kp = \SimpleStruct.opt!.i
  return s[keyPath: kp]
}

// CHECK-LABEL: sil {{.*}}testit
func testit() {
  // CHECK-OUTPUT: GenStructRead: 27
  print("GenStructRead: \(testGenStructRead(GenStruct(SimpleClass(27))).i)")

  // CHECK-OUTPUT: GenStructWrite: 28
  var s = GenStruct(SimpleClass(0))
  testGenStructWrite(&s, SimpleClass(28))
  print("GenStructWrite: \(s.st.i)")

  // CHECK-OUTPUT: GenStructModify: 38
  testGenStructModify(&s)
  print("GenStructModify: \(s.st.i)")

  // CHECK-OUTPUT: GenClassRead: 29
  print("GenClassRead: \(testGenClassRead(GenClass(SimpleClass(29))).i)")

  // CHECK-OUTPUT: DerivedClassRead: 12
  print("DerivedClassRead: \(testDerivedClassRead(DerivedClass<Int>())))")

  // CHECK-OUTPUT: DerivedClass2Read: 12
  print("DerivedClass2Read: \(testDerivedClass2Read(DerivedClass2())))")

  // CHECK-OUTPUT: GenClassWrite: 30
  let c = GenClass(SimpleClass(0))
  testGenClassWrite(c, SimpleClass(30))
  print("GenClassWrite: \(c.ct.i)")

  // CHECK-OUTPUT: GenClassModify: 40
  testGenClassModify(c)
  print("GenClassModify: \(c.ct.i)")

  // CHECK-OUTPUT: NestedRead1: 31
  print("NestedRead1: \(testNestedRead1(GenStruct(GenClass(GenStruct(SimpleClass(31))))))")

  // CHECK-OUTPUT: NestedRead2: 32
  print("NestedRead2: \(testNestedRead2(GenStruct(GenClass(GenStruct(SimpleClass(32))))).i)")

  // CHECK-OUTPUT: NestedWrite: 33
  let c2 = GenClass(GenStruct(SimpleClass(0)))
  testNestedWrite(c2, 33)
  print("NestedWrite: \(c2.ct.st.i)")

  // CHECK-OUTPUT: NestedModify: 44
  var s2 = GenStruct(GenClass(GenClass(SimpleClass(34))))
  testNestedModify(&s2)
  print("NestedModify: \(s2.st.ct.ct.i)")
  
  // CHECK-OUTPUT: Getter: 51
  var s3 = GenStruct(SimpleClass(50))
  print("Getter: \(testGetter(s3))")
  
  // CHECK-OUTPUT: ComputedModify: 61
  testComputedModify(&s3)
  print("ComputedModify: \(s3.computed)")
  
  // CHECK-OUTPUT: Subscript: [1, 3, 3, 4, 5]
  testSubscript(&s3)
  print("Subscript: \(s3.st.array)")
  
  var s4 = SimpleStruct()
  // CHECK-OUTPUT: Tuple: 1
  testTuple(&s4)
  print("Tuple: \(s4.tuple.0)")
  
  // CHECK-OUTPUT: OptionalChain1: nil
  print("OptionalChain1: \(String(describing: testOptionalChain(s4)))")
  
  // CHECK-OUTPUT: OptionalChain2: Optional(70)
  s4.opt = .init(i: 70)
  print("OptionalChain2: \(String(describing: testOptionalChain(s4)))")
  
  // CHECK-OUTPUT: OptionalForce: 80
  testModifyOptionalForce(&s4)
  print("OptionalForce: \(testGetOptionalForce(s4))")
}

testit()

// CHECK-OUTPUT: SimpleClass obj count: 0
print("SimpleClass obj count: \(SimpleClass.numObjs)")
// CHECK-OUTPUT: GenClass obj count: 0
print("GenClass obj count: \(numGenClassObjs)")


