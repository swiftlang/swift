// RUN: %swift -dump-cfg %s | FileCheck %s

func literals() {
  var a = 1
  var b = 1.1
  var c = 'x'
  var d = "foo"
}
// CHECK: func_decl literals
// CHECK: integerliteral 1, width=64
// CHECK: floatliteral 1.1
// CHECK: characterliteral 120
// CHECK: stringliteral "foo"



func bar(x:Int)

func call() {
  bar(42);
}

// CHECK: func_decl call

func tuples() {
  bar((4, 5).$1)
  
  var T1 : (a : Int16, b : Int) = (b = 42, a = 777)
  
  // varargs.
  var va1 : (Int...) = ()
  var va2 : (Int, Int...) = (1,2,3)
}

// CHECK: func_decl tuples


class C {}

// CHECK: func_decl classes
func classes() {
  // CHECK: constantref constructor
  var a = new C
}



func byrefcallee(x : [byref] Int)
func address_of_expr() {
  var x : Int = 4
  byrefcallee(&x)
}



func identity<T>(x : T) -> T

// CHECK: func_decl specialize_expr
func specialize_expr() -> Int {
  // CHECK: specialize {{.*}} -> (x : Int) -> Int
   return identity(17)
}


// CHECK: func_decl scalar_to_tuple
func scalar_to_tuple() {
  var a : (Int, Float = 1.0) = 42
  var b : (Int, Int = 12, Float = 1.0) = 42

  var c : (Int...) = 42
  var d : (Int, Int...) = 42
}

// CHECK: func_decl array_alloc
func array_alloc(n : Int) {
  var a : Int[] = new Int[n]
}

/*
struct SomeStruct {
  static func a() {}
}

func calls() {
  var a : SomeStruct
  a.a()
}
*/
