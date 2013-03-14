// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

func standalone_function(x:Int, y:Int) -> Int {
  return x
}

// -- Entry point BBs correspond to curried arguments in left-to-right order.
// CHECK: func_decl curried_function.1 : $(x : Int64)(y : Int64) -> Int64
func curried_function(x:Int)(y:Int) -> Int {
  // CHECK: bb0(%0 : Int64, %1 : Int64):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[YBOX:%[0-9]+]] = alloc_box $Int64

  return standalone_function(x, y)
  // CHECK: [[FUNC:%[0-9]+]] = constant_ref $(x : Int64, y : Int64) -> Int64, @standalone_function
  // CHECK: [[X:%[0-9]+]] = load [[XBOX]]#1
  // CHECK: [[Y:%[0-9]+]] = load [[YBOX]]#1
  // CHECK: apply [[FUNC]]([[X]], [[Y]])

  // CHECK: return
}

// -- Local function has extra uncurry level with context
// CHECK: func_decl <anonymous function>.1 : $(Builtin.ObjectPointer, [byref] Int64, Builtin.ObjectPointer, [byref] Int64)(z : Int64) -> Int64
// bb0(%0 : Builtin.ObjectPointer, %1 : *Int64, %2 : Builtin.ObjectPointer, %3 : *Int64, %4 : Int64):

// -- Curried function that returns a function uncurries to the right "natural" level
// CHECK: func_decl curried_function_returns_function.1 : $(x : Int64)(y : Int64) -> (z : Int64) -> Int64
func curried_function_returns_function(x:Int)(y:Int) -> (z:Int) -> Int {
  // CHECK: bb0(%0 : Int64, %1 : Int64):
  return func(z) { return standalone_function(standalone_function(x, y), z) }
}

class SomeClass {
  // -- Constructors and methods are uncurried in 'this'

  // CHECK: func_decl constructor.allocator.1 : $(SomeClass.metatype)(x : Int64, y : Int64) -> SomeClass
  // CHECK: bb0(%0 : SomeClass.metatype, %1 : Int64, %2 : Int64):
  // CHECK: func_decl constructor.initializer.1 : $(SomeClass)(x : Int64, y : Int64) -> SomeClass
  // CHECK: bb0(%0 : SomeClass, %1 : Int64, %2 : Int64):
  constructor(x:Int, y:Int) {}

  // CHECK: func_decl method.1 : $(SomeClass)(x : Int64) -> ()
  // CHECK: bb0(%0 : SomeClass, %1 : Int64):
  func method(x:Int) {}

  // CHECK: func_decl curried_method.2 : $(SomeClass)(x : Int64)(y : Int64) -> ()
  // CHECK: bb0(%0 : SomeClass, %1 : Int64, %2 : Int64):
  func curried_method(x:Int)(y:Int) {}

  // CHECK: func_decl static_method.1 : $(SomeClass.metatype)(x : Int64) -> ()
  // CHECK: bb0(%0 : SomeClass.metatype, %1 : Int64):
  static func static_method(x:Int) {}

  // CHECK: func_decl static_curried_method.2 : $(SomeClass.metatype)(x : Int64)(y : Int64) -> ()
  // CHECK: bb0(%0 : SomeClass.metatype, %1 : Int64, %2 : Int64):
  static func static_curried_method(x:Int)(y:Int) {}
}

func SomeClassWithBenefits() -> SomeClass.metatype {
  return SomeClass
}

// CHECK: func_decl calls : $(i : Int64, j : Int64, k : Int64) -> ()
func calls(i:Int, j:Int, k:Int) {
  // CHECK: bb0(%0 : Int64, %1 : Int64, %2 : Int64):
  // CHECK: [[IBOX:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[JBOX:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[KBOX:%[0-9]+]] = alloc_box $Int64

  // CHECK: [[FUNC:%[0-9]+]] = constant_ref $(x : Int64, y : Int64) -> Int64, @standalone_function
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[FUNC]]([[I]], [[J]])
  standalone_function(i, j)

  // CHECK: [[FUNC:%[0-9]+]] = constant_ref $(x : Int64)(y : Int64) -> Int64, @curried_function.1
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[FUNC]]([[I]], [[J]])
  curried_function(i)(j)

  // CHECK: [[FUNC1:%[0-9]+]] = constant_ref $(x : Int64)(y : Int64) -> (z : Int64) -> Int64, @curried_function_returns_function.1
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: [[FUNC2:%[0-9]+]] = apply [[FUNC1]]([[I]], [[J]])
  // CHECK: [[K:%[0-9]+]] = load [[KBOX]]#1
  // CHECK: apply [[FUNC2]]([[K]])
  curried_function_returns_function(i)(j)(k)

  // FIXME: use of curried entry points as values
  //var f1 = curried_function(i)
  //f1(j)
  //var f2 = curried_function
  //f2(i)(j)

  // CHECK: [[CBOX:%[0-9]+]] = alloc_box $SomeClass
  // CHECK: [[FUNC:%[0-9]+]] = constant_ref $(SomeClass.metatype)(x : Int64, y : Int64) -> SomeClass, @constructor.allocator.1
  // CHECK: [[META:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: [[C:%[0-9]+]] = apply [[FUNC]]([[META]], [[I]], [[J]])
  var c = new SomeClass(i, j)

  // -- Curry 'this' onto method argument lists dispatched using class_method.

  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]], @method.1
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: apply [[METHOD]]([[C]], [[I]])
  c.method(i)

  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]], @curried_method.2
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[METHOD]]([[C]], [[I]], [[J]])
  c.curried_method(i)(j)

  // -- Curry 'this' onto unapplied methods dispatched using class_method.

  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]], @method.1
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: apply [[METHOD]]([[C]], [[I]])
  SomeClass.method(c)(i)

  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]], @curried_method.2
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[METHOD]]([[C]], [[I]], [[J]])
  SomeClass.curried_method(c)(i)(j)

  // -- Curry 'this' onto unapplied methods, after applying side effects from a metatype expression.

  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]], @curried_method.2
  // CHECK: [[SIDEFUNC:%[0-9]+]] = constant_ref $() -> SomeClass.metatype, @SomeClassWithBenefits
  // CHECK: apply [[SIDEFUNC]]()
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[METHOD]]([[C]], [[I]], [[J]])
  SomeClassWithBenefits().curried_method(c)(i)(j)

  // -- Curry the metatype onto static method argument lists.
  // -- FIXME: Should get the metatype of the instance using a class_metatype instruction.
  
  // CHECK: [[META:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META]], @static_method.1
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: apply [[METHOD]]([[META]], [[I]])
  c.metatype.static_method(i)

  // CHECK: [[META:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META]], @static_curried_method.2
  // CHECK: [[I:%[0-9]+]] = load [[IBOX]]#1
  // CHECK: [[J:%[0-9]+]] = load [[JBOX]]#1
  // CHECK: apply [[METHOD]]([[META]], [[I]], [[J]])
  c.metatype.static_curried_method(i)(j)

  // FIXME: use of uncurried method entry points as values
  //var m1 = c.curried_method(i)
  //m1(j)
  //var m2 = c.curried_method
  //m2(i)(j)
  //var m3 = SomeClass.curried_method
  //m3(c)(i)(j)
  //var s1 = c.metatype.static_curried_method(i)
  //s1(j)
  //var s2 = c.metatype.static_curried_method
  //s2(i)(j)
}

