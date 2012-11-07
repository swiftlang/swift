// RUN: %swift -dump-cfg %s | FileCheck %s

// CHECK: typealias_decl
func  typealias_decl() {
  typealias a = Int
}

// CHECK: func_decl simple_patterns
func simple_patterns() {
  var _ = 4
  var _ : Int
}

// CHECK: func_decl named_pattern
func named_pattern() -> Int {
  var local_var : Int = 4
  
  var defaulted_var : Int  // Defaults to zero initialization

  return local_var + defaulted_var
}

func MRV() -> (Int, Float, (), Double)

// CHECK: func_decl tuple_patterns
func tuple_patterns() {
  // CHECK: alloc_var a
  // CHECK-NEXT: zerovalueinst Int
  // CHECK-NEXT: store{{.*}}initialization
  // CHECK: alloc_var b
  // CHECK-NEXT: zerovalueinst Float
  // CHECK-NEXT: store{{.*}}initialization
  var (a, b) : (Int, Float)


  // CHECK: tuple
  // CHECK-NEXT: alloc_var c
  // CHECK-NEXT: store{{.*}}initialization
  // CHECK-NEXT: alloc_var d
  // CHECK-NEXT: store{{.*}}initialization
  var (c, d) = (a, b)
  
  
  // CHECK: constantref MRV
  // CHECK-NEXT: apply
  // CHECK-NEXT: tupleelement{{.*}}, 0
  // CHECK-NEXT: alloc_var e
  // CHECK-NEXT: store{{.*}}initialization
  // CHECK-NEXT: tupleelement{{.*}}, 1
  // CHECK-NEXT: alloc_var f
  // CHECK-NEXT: store{{.*}}initialization
  // CHECK-NEXT: tupleelement{{.*}}, 2
  // CHECK-NEXT: alloc_var g
  // CHECK-NEXT: store{{.*}}initialization
  // CHECK-NEXT: tupleelement{{.*}}, 3
  // CHECK-NEXT: alloc_var h
  // CHECK-NEXT: store{{.*}}initialization
  var (e,f,g,h) = MRV()
}

// CHECK: func_decl simple_arguments
// CHECK: bb0(%0 : Int, %1 : Int):
// CHECK: alloc_var x
// CHECK-NEXT: store %0 -> {{.*}} [initialization]
// CHECK: alloc_var y
// CHECK-NEXT: store %1 -> {{.*}} [initialization]
func simple_arguments(x : Int, y : Int) -> Int {
  return x+y
}

// CHECK: func_decl curried_arguments
// CHECK: bb0(%0 : Int, %1 : Int):
// CHECK: alloc_var x
// CHECK-NEXT: store %0 -> {{.*}} [initialization]
// CHECK: alloc_var y
// CHECK-NEXT: store %1 -> {{.*}} [initialization]
func curried_arguments(x : Int) (y : Int) -> Int {
  return x+y
}

// CHECK: func_decl tuple_argument
// CHECK: bb0(%0 : (Int, Float, ())):
// CHECK: alloc_var x{{.*}}(Int, Float, ())
// CHECK-NEXT: store %0 -> {{.*}} [initialization]
func tuple_argument(x : (Int, Float, ())) {
}

// CHECK: func_decl byref_argument
// CHECK: bb0(%0 : [byref] Int, %1 : Int):
// CHECK-NOT: alloc_var x
// CHECK: alloc_var y
// CHECK: store {{.*}} -> %0
func byref_argument(x : [byref] Int, y : Int) {
  x = y
}

