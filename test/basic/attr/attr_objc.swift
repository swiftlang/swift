// RUN: %swift %s -verify
// RUN: %swift-ide-test -print-ast-typechecked -source-filename %s | FileCheck %s

class PlainClass {}
struct PlainStruct {}
enum PlainEnum {}
protocol PlainProtocol {}

@objc class Class_ObjC1 {}

@class_protocol
protocol Protocol_Class1 {} // expected-note {{protocol 'Protocol_Class1' declared here}}

@class_protocol
protocol Protocol_Class2 {}

@class_protocol @objc
protocol Protocol_ObjC1 {}

@class_protocol @objc
protocol Protocol_ObjC2 {}




//===--- Subjects of @objc attribute.

@objc
var subject_globalVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

var subject_getterSetter: Int {
@objc get: return 0 // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
@objc set: // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

class subject_getterSetter1 {
  var instanceVar1: Int {
  @objc get: return 0 // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  }

  var instanceVar2: Int {
  get: return 0
  @objc set: // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  }

  var instanceVar3: Int {
  @objc get: return 0 // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc set: // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  }
}

class subject_staticVar1 {
  @objc
  type var staticVar1: Int // expected-error {{type properties cannot be declared '@objc'}}
  // expected-error@-1 {{type variables not yet supported in classes}}
}

@objc
func subject_freeFunc() { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  var subject_localVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_nestedFreeFunc() { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  }
}

@objc
func subject_genericFunc<T>(t: T) { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  var subject_localVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

func subject_funcParam(a: @objc Int) { // expected-error {{attribute can only be applied to declarations, not types}}
}

@objc
struct subject_struct { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

@objc
struct subject_genericStruct<T> { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

@objc
class subject_class1 { // no-error
  @objc
  var subject_instanceVar: Int // no-error

  @objc
  func subject_instanceFunc() {} // no-error
}

@objc
class subject_class2 : Protocol_Class1, PlainProtocol { // no-error
}

@objc
class subject_genericClass<T> { // no-error
  @objc
  var subject_instanceVar: Int // no-error

  @objc
  init() {} // no-error

  @objc
  func subject_instanceFunc() {} // no-error
}

@objc
enum subject_enum { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  case subject_enumElement1 // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  case subject_enumElement2(Int) // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

@objc
enum subject_genericEnum<T> { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  @objc
  case subject_enumElement1 // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  case subject_enumElement2(Int) // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}


@objc
protocol subject_protocol1 { // expected-error {{only [class_protocol] protocols can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

@objc @class_protocol
protocol subject_protocol2 {} // no-error
// CHECK-LABEL: @objc @class_protocol protocol subject_protocol2 {

@class_protocol @objc
protocol subject_protocol3 {} // no-error
// CHECK-LABEL: @objc @class_protocol protocol subject_protocol3 {

@objc
protocol subject_protocol4 : PlainProtocol {} // expected-error {{only [class_protocol] protocols can be declared 'objc'}}

@objc
protocol subject_protocol5 : Protocol_Class1 {} // expected-error {{[objc] protocol 'subject_protocol5' cannot refine non-[objc] protocol 'Protocol_Class1'}}

@objc
protocol subject_protocol6 : Protocol_ObjC1 {}

protocol subject_containerProtocol1 {
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  func subject_instanceFunc() // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}

  @objc
  type func subject_staticFunc() // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
}

func concreteContext1() {
  @objc
  class subject_inConcreteContext {} // expected-error {{only classes at file scope can be declared 'objc'}}
}

class ConcreteContext2 {
  @objc
  class subject_inConcreteContext {} // expected-error {{only classes at file scope can be declared 'objc'}}
}

func genericContext1<T>() {
  @objc
  class subject_inGenericContext {} // expected-error {{only classes at file scope can be declared 'objc'}}

  class subject_constructor_inGenericContext {
    @objc
    init() {} // no-error
  }

  class subject_var_inGenericContext {
    @objc
    var subject_instanceVar: Int // no-error
  }

  class subject_func_inGenericContext {
    @objc
    func f() {} // no-error
  }
}

class GenericContext2<T> {
  @objc
  class subject_inGenericContext {} // expected-error {{only classes at file scope can be declared 'objc'}}

  @objc
  func f() {} // no-error
}

class GenericContext3<T> {
  class MoreNested {
    @objc
    class subject_inGenericContext {} // expected-error {{only classes at file scope can be declared 'objc'}}

    @objc
    func f() {} // no-error
  }
}


class subject_subscriptIndexed1 {
  @objc
  subscript(a: Int) -> Int { // no-error
  get: return 0
  }
}
class subject_subscriptIndexed2 {
  @objc
  subscript(a: Int8) -> Int { // no-error
  get: return 0
  }
}
class subject_subscriptIndexed3 {
  @objc
  subscript(a: UInt8) -> Int { // no-error
  get: return 0
  }
}

class subject_subscriptKeyed1 {
  @objc
  subscript(a: String) -> Int { // no-error
  get: return 0
  }
}
class subject_subscriptKeyed2 {
  @objc
  subscript(a: PlainClass) -> Int { // no-error
  get: return 0
  }
}
class subject_subscriptKeyed3 {
  @objc
  subscript(a: PlainClass.metatype) -> Int { // no-error
  get: return 0
  }
}
class subject_subscriptKeyed4 {
  @objc
  subscript(a: Protocol_ObjC1) -> Int { // no-error
  get: return 0
  }
}

class subject_subscriptInvalid1 {
  @objc
  subscript(a: Float32) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid2 {
  @objc
  subscript(a: PlainStruct) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid3 {
  @objc
  subscript(a: PlainEnum) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid4 {
  @objc
  subscript(a: PlainProtocol) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid5 {
  @objc
  subscript(a: Protocol_Class1) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid6 {
  @objc
  subscript(a: protocol<Protocol_Class1, Protocol_Class2>) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid7 {
  @objc
  subscript(a: protocol<Protocol_ObjC1, Protocol_ObjC2>) -> Int { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared 'objc'}}
  get: return 0
  }
}

//===--- Tests for @objc inference.

@objc
class infer_instanceFunc1 {
// CHECK-LABEL: @objc class infer_instanceFunc1 {

  func func1() {}
// CHECK-LABEL: /* @objc(inferred) */ func func1() {

  @objc func func1_() {} // no-error

  func func2(a: Int) {}
// CHECK-LABEL: /* @objc(inferred) */ func func2(a: Int) {

  @objc func func2_(a: Int) {} // no-error

  func func3(a: Int) -> Int {}
// CHECK-LABEL: /* @objc(inferred) */ func func3(a: Int) -> Int {

  @objc func func3_(a: Int) -> Int {} // no-error

  func func4(a: Int) b(b: Double) {}
// CHECK-LABEL: /* @objc(inferred) */ func func4(a: Int) b(b: Double) {

  @objc func func4_(a: Int) b(b: Double) {} // no-error

  func func5(a: String) {}
// CHECK-LABEL: /* @objc(inferred) */ func func5(a: String) {

  @objc func func5_(a: String) {} // no-error

  func func6() -> String {}
// CHECK-LABEL: /* @objc(inferred) */ func func6() -> String {

  @objc func func6_() -> String {} // no-error

  func func7(a: PlainClass) {}
// CHECK-LABEL: /* @objc(inferred) */ func func7(a: PlainClass) {

  @objc func func7_(a: PlainClass) {} // no-error

  func func8() -> PlainClass {}
// CHECK-LABEL: /* @objc(inferred) */ func func8() -> PlainClass {

  @objc func func8_() -> PlainClass {} // no-error

  func func9(a: PlainStruct) {}
// CHECK-LABEL: {{^}} func func9(a: PlainStruct) {

  @objc func func9_(a: PlainStruct) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  func func10() -> PlainStruct {}
// CHECK-LABEL: {{^}} func func10() -> PlainStruct {

  @objc func func10_() -> PlainStruct {}
  // expected-error@-1 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  func func11(a: PlainEnum) {}
// CHECK-LABEL: {{^}} func func11(a: PlainEnum) {

  @objc func func11_(a: PlainEnum) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift enums cannot be represented in Objective-C}}

  func func12(a: PlainProtocol) {}
// CHECK-LABEL: {{^}} func func12(a: PlainProtocol) {

  @objc func func12_(a: PlainProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  func func13(a: Class_ObjC1) {}
// CHECK-LABEL: /* @objc(inferred) */ func func13(a: Class_ObjC1) {

  @objc func func13_(a: Class_ObjC1) {} // no-error

  func func14(a: Protocol_Class1) {}
// CHECK-LABEL: {{^}} func func14(a: Protocol_Class1) {

  @objc func func14_(a: Protocol_Class1) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  func func15(a: Protocol_ObjC1) {}
// CHECK-LABEL: /* @objc(inferred) */ func func15(a: Protocol_ObjC1) {

  @objc func func15_(a: Protocol_ObjC1) {} // no-error

  func func16(a: DynamicLookup) {}
// CHECK-LABEL: /* @objc(inferred) */ func func16(a: DynamicLookup) {

  @objc func func16_(a: DynamicLookup) {} // no-error

  func func17(a: () -> ()) {}
// CHECK-LABEL: /* @objc(inferred) */ func func17(a: () -> ()) {

  @objc func func17_(a: () -> ()) {} // no-error

  func func18(a: (Int) -> ()) b(b: Int) {}
// CHECK-LABEL: /* @objc(inferred) */ func func18(a: (Int) -> ()) b(b: Int) {

  @objc func func18_(a: (Int) -> ()) b(b: Int) {} // no-error

  func func19(a: (String) -> ()) b(b: Int) {}
// CHECK-LABEL: /* @objc(inferred) */ func func19(a: (String) -> ()) b(b: Int) {

  @objc func func19_(a: (String) -> ()) b(b: Int) {} // no-error


  func func_FunctionReturn1() -> () -> () {}
// CHECK-LABEL: /* @objc(inferred) */ func func_FunctionReturn1() -> () -> () {

  @objc func func_FunctionReturn1_() -> () -> () {}

  func func_FunctionReturn2() -> (Int) -> () {}
// CHECK-LABEL: /* @objc(inferred) */ func func_FunctionReturn2() -> (Int) -> () {

  @objc func func_FunctionReturn2_() -> (Int) -> () {}

  func func_FunctionReturn3() -> () -> Int {}
// CHECK-LABEL: /* @objc(inferred) */ func func_FunctionReturn3() -> () -> Int {

  @objc func func_FunctionReturn3_() -> () -> Int {}

  func func_FunctionReturn4() -> (String) -> () {}
// CHECK-LABEL: /* @objc(inferred) */ func func_FunctionReturn4() -> (String) -> () {

  @objc func func_FunctionReturn4_() -> (String) -> () {}

  func func_FunctionReturn5() -> () -> String {}
// CHECK-LABEL: /* @objc(inferred) */ func func_FunctionReturn5() -> () -> String {

  @objc func func_FunctionReturn5_() -> () -> String {}


  func func_ZeroParams1() {}
// CHECK-LABEL: /* @objc(inferred) */ func func_ZeroParams1() {

  @objc func func_ZeroParams1() {} // no-error


  func func_OneParam1(a: Int) {}
// CHECK-LABEL: /* @objc(inferred) */ func func_OneParam1(a: Int) {

  @objc func func_OneParam1(a: Int) {} // no-error


  func func_TupleStyle1(a: Int, b: Int) {}
// CHECK-LABEL: {{^}} func func_TupleStyle1(a: Int, b: Int) {

  @objc func func_TupleStyle1(a: Int, b: Int) {}
  // expected-error@-1 {{method cannot be marked @objc because it is not using selector-style declaration syntax}}

  func func_TupleStyle2(a: Int, b: Int, c: Int) {}
// CHECK-LABEL: {{^}} func func_TupleStyle2(a: Int, b: Int, c: Int) {

  @objc func func_TupleStyle2(a: Int, b: Int, c: Int) {}
  // expected-error@-1 {{method cannot be marked @objc because it is not using selector-style declaration syntax}}


  func func_Curried1()() {}
// CHECK-LABEL: {{^}} func func_Curried1()() {

  @objc func func_Curried1_()() {}
  // expected-error@-1 {{method cannot be marked @objc because curried functions cannot be represented in Objective-C}}

  func func_Curried2()(a: Int) {}
// CHECK-LABEL: {{^}} func func_Curried2()(a: Int) {

  @objc func func_Curried2_()(a: Int) {}
  // expected-error@-1 {{method cannot be marked @objc because curried functions cannot be represented in Objective-C}}

  func func_Curried3()() -> Int {}
// CHECK-LABEL: {{^}} func func_Curried3()() -> Int {

  @objc func func_Curried3_()() -> Int {}
  // expected-error@-1 {{method cannot be marked @objc because curried functions cannot be represented in Objective-C}}

  func func_Curried4()(a: String) {}
// CHECK-LABEL: {{^}} func func_Curried4()(a: String) {

  @objc func func_Curried4_()(a: String) {}
  // expected-error@-1 {{method cannot be marked @objc because curried functions cannot be represented in Objective-C}}

  func func_Curried5()() -> String {}
// CHECK-LABEL: {{^}} func func_Curried5()() -> String {

  @objc func func_Curried5_()() -> String {}
  // expected-error@-1 {{method cannot be marked @objc because curried functions cannot be represented in Objective-C}}


  // Check that we produce diagnostics for every parameter and return type.
  @objc func func_MultipleDiags(a: PlainStruct) b(b: PlainEnum) -> protocol<> {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter 1 cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-error@-3 {{method cannot be marked @objc because the type of the parameter 2 cannot be represented in Objective-C}}
  // expected-note@-4 {{Swift enums cannot be represented in Objective-C}}
  // expected-error@-5 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-6 {{'protocol<>' is not considered '@objc'; use 'AnyObject' instead}}

  @objc func func_UnnamedParam1(_: Int) {} // no-error

  @objc func func_UnnamedParam2(_: PlainStruct) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
}

@objc
class infer_constructor1 {
// CHECK-LABEL: @objc class infer_constructor1

  init() {}
  // CHECK: /* @objc(inferred) */ init()

  init(a: Int) {}
  // CHECK: /* @objc(inferred) */ init(a: Int)
}

@objc
class infer_destructor1 {
// CHECK-LABEL: @objc class infer_destructor1

  destructor() {}
  // CHECK: /* @objc(inferred) */ destructor()
}

// @!objc
class infer_destructor2 {
// CHECK-LABEL: {{^}}class infer_destructor2

  destructor() {}
  // CHECK: /* @objc(inferred) */ destructor()
}

@objc
class infer_instanceVar1 {
// CHECK-LABEL: @objc class infer_instanceVar1 {

  var instanceVar1: Int
  // CHECK: var /* @objc(inferred) */ instanceVar1: Int

  var (instanceVar2, instanceVar3): (Int, PlainProtocol)
  // CHECK: var (/* @objc(inferred) */ instanceVar2, instanceVar3): (Int, PlainProtocol)

  @objc var (instanceVar1_, instanceVar2_): (Int, PlainProtocol)
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var intstanceVar4: Int {
  // CHECK: /* @objc(inferred) */ var intstanceVar4: Int
  get:
  // CHECK-NEXT: /* @objc(inferred) */ get:
    return 0
  }

  var intstanceVar5: Int {
  // CHECK: /* @objc(inferred) */ var intstanceVar5: Int
  get:
  // CHECK-NEXT: /* @objc(inferred) */ get:
  set:
  // CHECK: /* @objc(inferred) */ set:
  }

  @objc var instanceVar5_: Int {
  // CHECK: @objc var instanceVar5_: Int
  get:
  // CHECK-NEXT: /* @objc(inferred) */ get:
  set:
  // CHECK: /* @objc(inferred) */ set:
  }

  var var_Int: Int
// CHECK-LABEL: var /* @objc(inferred) */ var_Int: Int

  var var_Bool: Bool
// CHECK-LABEL: var /* @objc(inferred) */ var_Bool: Bool

  var var_CBool: CBool
// CHECK-LABEL: var /* @objc(inferred) */ var_CBool: CBool

  var var_String: String
// CHECK-LABEL: var /* @objc(inferred) */ var_String: String

  var var_Float: Float
  var var_Double: Double
// CHECK-LABEL: var /* @objc(inferred) */ var_Float: Float
// CHECK-LABEL: var /* @objc(inferred) */ var_Double: Double

  var var_Char: UnicodeScalar
// CHECK-LABEL: var /* @objc(inferred) */ var_Char: UnicodeScalar

  //===--- Tuples.

  var var_tuple1: ()
// CHECK-LABEL: var var_tuple1: ()

  @objc var var_tuple1_: ()
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{empty tuple type cannot be represented in Objective-C}}

  var var_tuple2: Void
// CHECK-LABEL: var var_tuple2: Void

  @objc var var_tuple2_: Void
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{empty tuple type cannot be represented in Objective-C}}

  var var_tuple3: (Int)
// CHECK-LABEL: var /* @objc(inferred) */ var_tuple3: (Int)

  @objc var var_tuple3_: (Int) // no-error

  var var_tuple4: (Int, Int)
// CHECK-LABEL: var var_tuple4: (Int, Int)

  @objc var var_tuple4_: (Int, Int)
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{tuples cannot be represented in Objective-C}}

  //===--- Stdlib integer types.

  var var_Int8: Int8
  var var_Int16: Int16
  var var_Int32: Int32
  var var_Int64: Int64
// CHECK-LABEL: var /* @objc(inferred) */ var_Int8: Int8
// CHECK-LABEL: var /* @objc(inferred) */ var_Int16: Int16
// CHECK-LABEL: var /* @objc(inferred) */ var_Int32: Int32
// CHECK-LABEL: var /* @objc(inferred) */ var_Int64: Int64

  var var_UInt8: UInt8
  var var_UInt16: UInt16
  var var_UInt32: UInt32
  var var_UInt64: UInt64
// CHECK-LABEL: var /* @objc(inferred) */ var_UInt8: UInt8
// CHECK-LABEL: var /* @objc(inferred) */ var_UInt16: UInt16
// CHECK-LABEL: var /* @objc(inferred) */ var_UInt32: UInt32
// CHECK-LABEL: var /* @objc(inferred) */ var_UInt64: UInt64

  var var_COpaquePointer: COpaquePointer
// CHECK-LABEL: var /* @objc(inferred) */ var_COpaquePointer: COpaquePointer

  var var_PlainClass: PlainClass
// CHECK-LABEL: var /* @objc(inferred) */ var_PlainClass: PlainClass

  @objc var var_PlainClass_: PlainClass // no-error

  var var_PlainStruct: PlainStruct
// CHECK-LABEL: var var_PlainStruct: PlainStruct

  @objc var var_PlainStruct_: PlainStruct
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  var var_PlainEnum: PlainEnum
// CHECK-LABEL: var var_PlainEnum: PlainEnum

  @objc var var_PlainEnum_: PlainEnum
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift enums cannot be represented in Objective-C}}

  var var_PlainProtocol: PlainProtocol
// CHECK-LABEL: var var_PlainProtocol: PlainProtocol

  @objc var var_PlainProtocol_: PlainProtocol
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_ClassObjC: Class_ObjC1
// CHECK-LABEL: var /* @objc(inferred) */ var_ClassObjC: Class_ObjC1

  @objc var var_ClassObjC_: Class_ObjC1 // no-error

  var var_ProtocolClass: Protocol_Class1
// CHECK-LABEL: var var_ProtocolClass: Protocol_Class1

  @objc var var_ProtocolClass_: Protocol_Class1
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_ProtocolObjC: Protocol_ObjC1
// CHECK-LABEL: var /* @objc(inferred) */ var_ProtocolObjC: Protocol_ObjC1

  @objc var var_ProtocolObjC_: Protocol_ObjC1 // no-error


  var var_PlainClassMetatype: PlainClass.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_PlainClassMetatype: PlainClass.metatype

  @objc var var_PlainClassMetatype_: PlainClass.metatype // no-error

  var var_PlainStructMetatype: PlainStruct.metatype
// CHECK-LABEL: var var_PlainStructMetatype: PlainStruct.metatype

  @objc var var_PlainStructMetatype_: PlainStruct.metatype
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_PlainEnumMetatype: PlainEnum.metatype
// CHECK-LABEL: var var_PlainEnumMetatype: PlainEnum.metatype

  @objc var var_PlainEnumMetatype_: PlainEnum.metatype
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_PlainProtocolMetatype: PlainProtocol.metatype
// CHECK-LABEL: var var_PlainProtocolMetatype: PlainProtocol.metatype

  @objc var var_PlainProtocolMetatype_: PlainProtocol.metatype
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_ClassObjCMetatype: Class_ObjC1.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_ClassObjCMetatype: Class_ObjC1.metatype

  @objc var var_ClassObjCMetatype_: Class_ObjC1.metatype // no-error

  var var_ProtocolClassMetatype: Protocol_Class1.metatype
// CHECK-LABEL: var var_ProtocolClassMetatype: Protocol_Class1.metatype

  @objc var var_ProtocolClassMetatype_: Protocol_Class1.metatype
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_ProtocolObjCMetatype: Protocol_ObjC1.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_ProtocolObjCMetatype: Protocol_ObjC1.metatype

  @objc var var_ProtocolObjCMetatype_: Protocol_ObjC1.metatype // no-error


  var var_DynamicLookup1: DynamicLookup
  var var_DynamicLookup2: DynamicLookup.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_DynamicLookup1: DynamicLookup
// CHECK-LABEL: var /* @objc(inferred) */ var_DynamicLookup2: DynamicLookup.metatype

  var var_Existential0: protocol<>
// CHECK-LABEL: var var_Existential0: protocol<>

  @objc var var_Existential0_: protocol<>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{'protocol<>' is not considered '@objc'; use 'AnyObject' instead}}

  var var_Existential1: protocol<PlainProtocol>
// CHECK-LABEL: var var_Existential1: protocol<PlainProtocol>

  @objc var var_Existential1_: protocol<PlainProtocol>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential2: protocol<PlainProtocol, PlainProtocol>
// CHECK-LABEL: var var_Existential2: protocol<PlainProtocol, PlainProtocol>

  @objc var var_Existential2_: protocol<PlainProtocol, PlainProtocol>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential3: protocol<PlainProtocol, Protocol_Class1>
// CHECK-LABEL: var var_Existential3: protocol<PlainProtocol, Protocol_Class1>

  @objc var var_Existential3_: protocol<PlainProtocol, Protocol_Class1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential4: protocol<PlainProtocol, Protocol_ObjC1>
// CHECK-LABEL: var var_Existential4: protocol<PlainProtocol, Protocol_ObjC1>

  @objc var var_Existential4_: protocol<PlainProtocol, Protocol_ObjC1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential5: protocol<Protocol_Class1>
// CHECK-LABEL: var var_Existential5: protocol<Protocol_Class1>

  @objc var var_Existential5_: protocol<Protocol_Class1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential6: protocol<Protocol_Class1, Protocol_Class2>
// CHECK-LABEL: var var_Existential6: protocol<Protocol_Class1, Protocol_Class2>

  @objc var var_Existential6_: protocol<Protocol_Class1, Protocol_Class2>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential7: protocol<Protocol_Class1, Protocol_ObjC1>
// CHECK-LABEL: var var_Existential7: protocol<Protocol_Class1, Protocol_ObjC1>

  @objc var var_Existential7_: protocol<Protocol_Class1, Protocol_ObjC1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential8: protocol<Protocol_ObjC1>
// CHECK-LABEL: var /* @objc(inferred) */ var_Existential8: protocol<Protocol_ObjC1>

  @objc var var_Existential8_: protocol<Protocol_ObjC1> // no-error

  var var_Existential9: protocol<Protocol_ObjC1, Protocol_ObjC2>
// CHECK-LABEL: var /* @objc(inferred) */ var_Existential9: protocol<Protocol_ObjC1, Protocol_ObjC2>

  @objc var var_Existential9_: protocol<Protocol_ObjC1, Protocol_ObjC2> // no-error


  var var_ExistentialMetatype0: protocol<>.metatype
  var var_ExistentialMetatype1: protocol<PlainProtocol>.metatype
  var var_ExistentialMetatype2: protocol<PlainProtocol, PlainProtocol>.metatype
  var var_ExistentialMetatype3: protocol<PlainProtocol, Protocol_Class1>.metatype
  var var_ExistentialMetatype4: protocol<PlainProtocol, Protocol_ObjC1>.metatype
  var var_ExistentialMetatype5: protocol<Protocol_Class1>.metatype
  var var_ExistentialMetatype6: protocol<Protocol_Class1, Protocol_Class2>.metatype
  var var_ExistentialMetatype7: protocol<Protocol_Class1, Protocol_ObjC1>.metatype
  var var_ExistentialMetatype8: protocol<Protocol_ObjC1>.metatype
  var var_ExistentialMetatype9: protocol<Protocol_ObjC1, Protocol_ObjC2>.metatype
// CHECK-LABEL: var var_ExistentialMetatype0: protocol<>.metatype
// CHECK-LABEL: var var_ExistentialMetatype1: protocol<PlainProtocol>.metatype
// CHECK-LABEL: var var_ExistentialMetatype2: protocol<PlainProtocol, PlainProtocol>.metatype
// CHECK-LABEL: var var_ExistentialMetatype3: protocol<PlainProtocol, Protocol_Class1>.metatype
// CHECK-LABEL: var var_ExistentialMetatype4: protocol<PlainProtocol, Protocol_ObjC1>.metatype
// CHECK-LABEL: var var_ExistentialMetatype5: protocol<Protocol_Class1>.metatype
// CHECK-LABEL: var var_ExistentialMetatype6: protocol<Protocol_Class1, Protocol_Class2>.metatype
// CHECK-LABEL: var var_ExistentialMetatype7: protocol<Protocol_Class1, Protocol_ObjC1>.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_ExistentialMetatype8: protocol<Protocol_ObjC1>.metatype
// CHECK-LABEL: var /* @objc(inferred) */ var_ExistentialMetatype9: protocol<Protocol_ObjC1, Protocol_ObjC2>.metatype


  var var_UnsafePointer1: UnsafePointer<Int>
  var var_UnsafePointer2: UnsafePointer<Bool>
  var var_UnsafePointer3: UnsafePointer<CBool>
  var var_UnsafePointer4: UnsafePointer<String>
  var var_UnsafePointer5: UnsafePointer<Float>
  var var_UnsafePointer6: UnsafePointer<Double>
  var var_UnsafePointer7: UnsafePointer<COpaquePointer>
  var var_UnsafePointer8: UnsafePointer<PlainClass>
  var var_UnsafePointer9: UnsafePointer<PlainStruct>
  var var_UnsafePointer10: UnsafePointer<PlainEnum>
  var var_UnsafePointer11: UnsafePointer<PlainProtocol>
  var var_UnsafePointer12: UnsafePointer<DynamicLookup>
  var var_UnsafePointer13: UnsafePointer<DynamicLookup.metatype>
  var var_UnsafePointer100: UnsafePointer<()>
  var var_UnsafePointer101: UnsafePointer<Void>
  var var_UnsafePointer102: UnsafePointer<(Int, Int)>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer1: UnsafePointer<Int>
// CHECK-LABEL: var var_UnsafePointer2: UnsafePointer<Bool>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer3: UnsafePointer<CBool>
// CHECK-LABEL: var var_UnsafePointer4: UnsafePointer<String>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer5: UnsafePointer<Float>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer6: UnsafePointer<Double>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer7: UnsafePointer<COpaquePointer>
// CHECK-LABEL: var var_UnsafePointer8: UnsafePointer<PlainClass>
// CHECK-LABEL: var var_UnsafePointer9: UnsafePointer<PlainStruct>
// CHECK-LABEL: var var_UnsafePointer10: UnsafePointer<PlainEnum>
// CHECK-LABEL: var var_UnsafePointer11: UnsafePointer<PlainProtocol>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer12: UnsafePointer<DynamicLookup>
// CHECK-LABEL: var /* @objc(inferred) */ var_UnsafePointer13: UnsafePointer<DynamicLookup.metatype>
// CHECK-LABEL: var var_UnsafePointer100: UnsafePointer<()>
// CHECK-LABEL: var var_UnsafePointer101: UnsafePointer<Void>
// CHECK-LABEL: var var_UnsafePointer102: UnsafePointer<(Int, Int)>


  var var_FunctionType1: () -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType1: () -> ()

  var var_FunctionType2: (Int) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType2: (Int) -> ()

  var var_FunctionType3: (Int) -> Int
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType3: (Int) -> Int

  var var_FunctionType4: (Int, Double) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType4: (Int, Double) -> ()

  var var_FunctionType5: (String) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType5: (String) -> ()

  var var_FunctionType6: () -> String
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType6: () -> String

  var var_FunctionType7: (PlainClass) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType7: (PlainClass) -> ()

  var var_FunctionType8: () -> PlainClass
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType8: () -> PlainClass

  var var_FunctionType9: (PlainStruct) -> ()
// CHECK-LABEL: var var_FunctionType9: (PlainStruct) -> ()

  var var_FunctionType10: () -> PlainStruct
// CHECK-LABEL: var var_FunctionType10: () -> PlainStruct

  var var_FunctionType11: (PlainEnum) -> ()
// CHECK-LABEL: var var_FunctionType11: (PlainEnum) -> ()

  var var_FunctionType12: (PlainProtocol) -> ()
// CHECK-LABEL: var var_FunctionType12: (PlainProtocol) -> ()

  var var_FunctionType13: (Class_ObjC1) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType13: (Class_ObjC1) -> ()

  var var_FunctionType14: (Protocol_Class1) -> ()
// CHECK-LABEL: var var_FunctionType14: (Protocol_Class1) -> ()

  var var_FunctionType15: (Protocol_ObjC1) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType15: (Protocol_ObjC1) -> ()

  var var_FunctionType16: (DynamicLookup) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType16: (DynamicLookup) -> ()

  var var_FunctionType17: (() -> ()) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType17: (() -> ()) -> ()

  var var_FunctionType18: ((Int) -> (), Int) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType18: ((Int) -> (), Int) -> ()

  var var_FunctionType19: ((String) -> (), Int) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionType19: ((String) -> (), Int) -> ()


  var var_FunctionTypeReturn1: () -> () -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionTypeReturn1: () -> () -> ()

  @objc var var_FunctionTypeReturn1_: () -> () -> () // no-error

  var var_FunctionTypeReturn2: () -> (Int) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionTypeReturn2: () -> (Int) -> ()

  @objc var var_FunctionTypeReturn2_: () -> (Int) -> () // no-error

  var var_FunctionTypeReturn3: () -> () -> Int
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionTypeReturn3: () -> () -> Int

  @objc var var_FunctionTypeReturn3_: () -> () -> Int // no-error

  var var_FunctionTypeReturn4: () -> (String) -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionTypeReturn4: () -> (String) -> ()

  @objc var var_FunctionTypeReturn4_: () -> (String) -> () // no-error

  var var_FunctionTypeReturn5: () -> () -> String
// CHECK-LABEL: var /* @objc(inferred) */ var_FunctionTypeReturn5: () -> () -> String

  @objc var var_FunctionTypeReturn5_: () -> () -> String // no-error


  var var_ThinFunctionType1: @thin () -> ()
// CHECK-LABEL: var var_ThinFunctionType1: @thin () -> ()

  @objc var var_ThinFunctionType1_: @thin () -> ()
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_BlockFunctionType1: @objc_block () -> ()
// CHECK-LABEL: var /* @objc(inferred) */ var_BlockFunctionType1: @objc_block () -> ()

  @objc var var_BlockFunctionType1_: @objc_block () -> () // no-error
}

@objc
class infer_instanceVar2<
    GP_Unconstrained,
    GP_PlainClass : PlainClass,
    GP_PlainProtocol : PlainProtocol,
    GP_Class_ObjC : Class_ObjC1,
    GP_Protocol_Class : Protocol_Class1,
    GP_Protocol_ObjC : Protocol_ObjC1> {
// CHECK-LABEL: @objc class infer_instanceVar2<{{.*}}> {

  var var_GP_Unconstrained: GP_Unconstrained
// CHECK-LABEL: var var_GP_Unconstrained: GP_Unconstrained

  @objc var var_GP_Unconstrained_: GP_Unconstrained
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_PlainClass: GP_PlainClass
// CHECK-LABEL: var var_GP_PlainClass: GP_PlainClass

  @objc var var_GP_PlainClass_: GP_PlainClass
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_PlainProtocol: GP_PlainProtocol
// CHECK-LABEL: var var_GP_PlainProtocol: GP_PlainProtocol

  @objc var var_GP_PlainProtocol_: GP_PlainProtocol
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Class_ObjC: GP_Class_ObjC
// CHECK-LABEL: var var_GP_Class_ObjC: GP_Class_ObjC

  @objc var var_GP_Class_ObjC_: GP_Class_ObjC
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Protocol_Class: GP_Protocol_Class
// CHECK-LABEL: var var_GP_Protocol_Class: GP_Protocol_Class

  @objc var var_GP_Protocol_Class_: GP_Protocol_Class
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Protocol_ObjC: GP_Protocol_ObjC
// CHECK-LABEL: var var_GP_Protocol_ObjC: GP_Protocol_ObjC

  @objc var var_GP_Protocol_ObjC: GP_Protocol_ObjC
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  func func_GP_Unconstrained(a: GP_Unconstrained) {}
// CHECK-LABEL: {{^}} func func_GP_Unconstrained(a: GP_Unconstrained) {

  @objc func func_GP_Unconstrained_(a: GP_Unconstrained) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}
}

class infer_instanceVar3 : Class_ObjC1 {
// CHECK-LABEL: /* @objc(inferred) */ class infer_instanceVar3 : Class_ObjC1 {

  var v1: Int
// CHECK-LABEL: var /* @objc(inferred) */ v1: Int
}


@objc @class_protocol
protocol infer_instanceVar4 {
// CHECK-LABEL: @objc @class_protocol protocol infer_instanceVar4 {

  var v1: Int
// CHECK-LABEL: var /* @objc(inferred) */ v1: Int
}

// @!objc
class infer_instanceVar5 {
// CHECK-LABEL: {{^}}class infer_instanceVar5 {

  @objc
  var intstanceVar1: Int {
  // CHECK: @objc var intstanceVar1: Int
  get:
  // CHECK: /* @objc(inferred) */ get:
  set:
  // CHECK: /* @objc(inferred) */ set:
  }
}

@objc
class infer_staticVar1 {
// CHECK-LABEL: @objc class infer_staticVar1 {

  type var staticVar1: Int // expected-error {{type variables not yet supported in classes}}
  // CHECK: {{^}} type var staticVar1: Int
}

// @!objc
class infer_subscript1 {
// CHECK-LABEL: class infer_subscript1

  @objc
  subscript(i: Int) -> Int {
  // CHECK: @objc subscript (i: Int) -> Int
  get:
  // CHECK: /* @objc(inferred) */ get:
  set:
  // CHECK: /* @objc(inferred) */ set:
  }
}


@class_protocol @objc
protocol infer_throughConformanceProto1 {
// CHECK-LABEL: @objc @class_protocol protocol infer_throughConformanceProto1 {

  func funcObjC1()
  var varObjC1: Int
  // CHECK: /* @objc(inferred) */ func funcObjC1()
  // CHECK: var /* @objc(inferred) */ varObjC1: Int

  func funcNonObjC1() -> PlainStruct
  var varNonObjC1: PlainStruct
  // CHECK: {{^}} func funcNonObjC1() -> PlainStruct
  // CHECK: {{^}} var varNonObjC1: PlainStruct
}

class infer_throughConformance1 : infer_throughConformanceProto1 {
// CHECK-LABEL: {{^}}class infer_throughConformance1 : infer_throughConformanceProto1 {
  func funcObjC1() {}
  var varObjC1: Int
  // CHECK: /* @objc(inferred) */ func funcObjC1() {
  // CHECK: var /* @objc(inferred) */ varObjC1: Int

  func funcNonObjC1() -> PlainStruct { return PlainStruct() }
  var varNonObjC1: PlainStruct
  // CHECK: func funcNonObjC1() -> PlainStruct {
  // CHECK: var varNonObjC1: PlainStruct

  // CHECK: /* @objc(inferred) */ destructor()  {
}


class infer_class1 : PlainClass {}
// CHECK-LABEL: {{^}}class infer_class1 : PlainClass {

class infer_class2 : Class_ObjC1 {}
// CHECK-LABEL: /* @objc(inferred) */ class infer_class2 : Class_ObjC1 {

class infer_class3 : infer_class2 {}
// CHECK-LABEL: /* @objc(inferred) */ class infer_class3 : infer_class2 {

class infer_class4 : Protocol_Class1 {}
// CHECK-LABEL: {{^}}class infer_class4 : Protocol_Class1 {

class infer_class5 : Protocol_ObjC1 {}
// CHECK-LABEL: {{^}}class infer_class5 : Protocol_ObjC1 {


protocol infer_protocol1 {}
// CHECK-LABEL: {{^}}protocol infer_protocol1 {

protocol infer_protocol2 : Protocol_Class1 {}
// CHECK-LABEL: {{^}}protocol infer_protocol2 : Protocol_Class1 {

protocol infer_protocol3 : Protocol_ObjC1 {}
// CHECK-LABEL: {{^}}protocol infer_protocol3 : Protocol_ObjC1 {

protocol infer_protocol4 : Protocol_Class1, Protocol_ObjC1 {}
// CHECK-LABEL: {{^}}protocol infer_protocol4 : Protocol_Class1, Protocol_ObjC1 {

protocol infer_protocol5 : Protocol_ObjC1, Protocol_Class1 {}
// CHECK-LABEL: {{^}}protocol infer_protocol5 : Protocol_ObjC1, Protocol_Class1 {

class C {
  // Don't crash.
  @objc func foo(x: Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
  @IBAction func myAction(sender : Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
}

//===---
//===--- @IBOutlet implies @objc
//===---

class HasIBOutlet {
// CHECK-LABEL: {{^}}class HasIBOutlet {

  @IBOutlet
  var goodOutlet: Class_ObjC1
  // CHECK-LABEL: var /* @objc(inferred) */ goodOutlet: Class_ObjC1

  @IBOutlet
  var badOutlet: PlainStruct
  // expected-error@-1 {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // CHECK-LABEL: {{^}}  var badOutlet: PlainStruct
}

