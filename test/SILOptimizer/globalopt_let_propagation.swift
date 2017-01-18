// RUN: %target-swift-frontend  -O -emit-sil -primary-file %s | %FileCheck %s

// Check that values of static let and global let variables are propagated into their uses
// and enable further optimizations like constant propagation, simplifications, etc.

// Define some global let variables.

// Currently GlobalOpt cannot deal with the new alloc_global instruction.

let PI = 3.1415
let ONE = 1.000
let I = 100
let J = 200
let S = "String1"

let VOLUME1 = I * J
let VOLUME2 = J * 2
let VOLUME3 = I + 10
 


struct IntWrapper1 {
  let val: Int
}

struct IntWrapper2 {
  let val: IntWrapper1
}

struct IntWrapper3 {
  let val: IntWrapper2
}

struct IntWrapper4 {
  let val:  IntWrapper2
  let val2: IntWrapper1
}

// Test with an initializer, where a SIL debug_value instruction might block
// analysis of the initializer and inhibit optimization of the let.
struct IntWrapper5 {
  let val: Int
  init(val: Int) { self.val = val }
  static let Five = IntWrapper5(val: 5)
}

var PROP1: Double {
   return PI
}

var PROP2: Int {
   return I * J - I
}

var VPI = 3.1415
var VI = 100
var VS = "String2"

// Define some static let variables inside a struct.
struct B {
 static let PI = 3.1415

 static let ONE = 1.000
 
 static let I = 100
 
 static let J = 200
 
 static let S1 = "String3"

 static let VOLUME1 = I * J

 static let VOLUME2 = J * 2

 static let VOLUME3 = I + 10
 
 static var PROP1: Double { 
   return PI
 }
 
 static var PROP2: Int { 
   return I * J - I
 }

 static func foo() {}

 static let IW3 = IntWrapper3(val: IntWrapper2(val: IntWrapper1(val: 10)))

 static let IW4 = IntWrapper4(val: IntWrapper2(val: IntWrapper1(val: 10)), val2: IntWrapper1(val: 100))

 static let IT1 = ((10, 20), 30, 40)

 static let IT2 = (100, 200, 300)
}

// Define some static let variables inside a class.
class C {
 static let PI = 3.1415

 static let ONE = 1.000
 
 static let I = 100
 
 static let J = 200
 
 static let S1 = "String3"

 static let VOLUME1 = I * J

 static let VOLUME2 = J * 2

 static let VOLUME3 = I + 10
 
 static var PROP1: Double { 
   return PI
 }
 
 static var PROP2: Int { 
   return I * J - I
 }

 static func foo() {}

 static let IW3 = IntWrapper3(val: IntWrapper2(val: IntWrapper1(val: 10)))

 static let IW4 = IntWrapper4(val: IntWrapper2(val: IntWrapper1(val: 10)), val2: IntWrapper1(val: 100))

 static let IT1 = ((10, 20), 30, 40)

 static let IT2 = (100, 200, 300)
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation15test_let_doubleFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_let_double() -> Double {
  return PI + 1.0
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation12test_let_intFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_let_int() -> Int {
  return I + 1
}

@inline(never)
public func test_let_string() -> String {
  return S
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation23test_let_double_complexFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_let_double_complex() -> Double {
  return PI + ONE + PROP1
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation20test_let_int_complexFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_let_int_complex() -> Int {
  return I + J + VOLUME1 + VOLUME2 + VOLUME3 + PROP2
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation29test_static_struct_let_doubleFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_double() -> Double {
  return B.PI + 1.0
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation26test_static_struct_let_intFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_int() -> Int {
  return B.I + 1
}

@inline(never)
public func test_static_struct_let_string() -> String {
  return B.S1
}


// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation37test_static_struct_let_double_complexFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_double_complex() -> Double {
  return B.PI + B.ONE + B.PROP1
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation34test_static_struct_let_int_complexFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_int_complex() -> Int {
  return B.I + B.J + B.VOLUME1 + B.VOLUME2 + B.VOLUME3 + B.PROP2
}


// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation28test_static_class_let_doubleFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_class_let_double() -> Double {
  return C.PI + 1.0
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation25test_static_class_let_intFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_class_let_int() -> Int {
  return C.I + 1
}

@inline(never)
public func test_static_class_let_string() -> String {
  return C.S1
}


// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation36test_static_class_let_double_complexFT_Sd
// CHECK: bb0:
// CHECK-NEXT: float_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_class_let_double_complex() -> Double {
  return C.PI + C.ONE + C.PROP1
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation33test_static_class_let_int_complexFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_class_let_int_complex() -> Int {
  return C.I + C.J + C.VOLUME1 + C.VOLUME2 + C.VOLUME3 + C.PROP2
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation15test_var_doubleFT_Sd
// CHECK: bb0:
// CHECK-NEXT: global_addr
// CHECK-NEXT: struct_element_addr
// CHECK-NEXT: load
@inline(never)
public func test_var_double() -> Double {
  return VPI + 1.0 
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation12test_var_intFT_Si
// CHECK: bb0: 
// CHECK-NEXT: global_addr
// CHECK-NEXT: struct_element_addr
// CHECK-NEXT: load
@inline(never)
public func test_var_int() -> Int {
  return VI + 1
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation33test_static_class_let_wrapped_intFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_class_let_wrapped_int() -> Int {
  return C.IW3.val.val.val + 1
}

// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation34test_static_struct_let_wrapped_intFT_Si
// CHECK: bb0:
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_wrapped_int() -> Int {
  return B.IW3.val.val.val + 1
}

// Test accessing multiple Int fields wrapped into multiple structs, where each struct may have
// multiple fields.
// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation51test_static_struct_let_struct_wrapped_multiple_intsFT_Si
// CHECK: bb0:
// CHECK-NOT: global_addr
// CHECK: integer_literal
// CHECK-NOT: global_addr
// CHECK: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_struct_wrapped_multiple_ints() -> Int {
  return B.IW4.val.val.val + B.IW4.val2.val + IntWrapper5.Five.val + 1
}

// Test accessing multiple Int fields wrapped into multiple structs, where each struct may have
// multiple fields.
// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation50test_static_class_let_struct_wrapped_multiple_intsFT_Si
// CHECK: bb0:
// CHECK-NOT: global_addr
// CHECK: integer_literal
// CHECK-NOT: global_addr
// CHECK: struct
// CHECK: return
@inline(never)
public func test_static_class_let_struct_wrapped_multiple_ints() -> Int {
  return C.IW4.val.val.val + C.IW4.val2.val + IntWrapper5.Five.val + 1
}

// Test accessing multiple Int fields wrapped into multiple tuples, where each tuple may have
// multiple fields.
// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation41test_static_struct_let_tuple_wrapped_intsFT_Si
// CHECK: bb0:
// CHECK-NOT: global_addr
// CHECK: integer_literal
// CHECK: struct
// CHECK: return
@inline(never)
public func test_static_struct_let_tuple_wrapped_ints() -> Int {
  return B.IT1.0.0 + B.IT2.1
}

// Test accessing multiple Int fields wrapped into multiple tuples, where each tuple may have
// multiple fields.
// CHECK-LABEL: sil [noinline] @_TF25globalopt_let_propagation40test_static_class_let_tuple_wrapped_intsFT_Si
// CHECK: bb0:
// CHECK-NOT: global_addr
// CHECK: integer_literal
// CHECK: struct
// CHECK: return
@inline(never)
public func test_static_class_let_tuple_wrapped_ints() -> Int {
  return C.IT1.0.0 + C.IT2.1
}
