// RUN: %target-swift-frontend -disable-func-sig-opts -O -emit-sil -primary-file %s | FileCheck %s

// Check that values of static let and global let variables are propagated into their uses
// and enable further optimizations like constant propagation, simplifications, etc.

// Define some global let variables.
let PI = 3.1415
let ONE = 1.000
let I = 100
let J = 200
let S = "String1"

let VOLUME1 = I * J
let VOLUME2 = J * 2
let VOLUME3 = I + 10
 
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
