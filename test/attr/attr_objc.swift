// RUN: %target-parse-verify-swift
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true | FileCheck %s
// RUN: not %target-swift-frontend -parse -dump-ast -disable-objc-attr-requires-foundation-module %s 2> %t.dump
// RUN: FileCheck -check-prefix CHECK-DUMP %s < %t.dump
// REQUIRES: objc_interop

import Foundation

class PlainClass {}
struct PlainStruct {}
enum PlainEnum {}
protocol PlainProtocol {} // expected-note {{protocol 'PlainProtocol' declared here}}


@objc class Class_ObjC1 {}

protocol Protocol_Class1 : class {} // expected-note {{protocol 'Protocol_Class1' declared here}}

protocol Protocol_Class2 : class {}

@objc protocol Protocol_ObjC1 {}

@objc protocol Protocol_ObjC2 {}




//===--- Subjects of @objc attribute.

@objc  
var subject_globalVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

var subject_getterSetter: Int {
  @objc 
  get { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
    return 0
  }
  @objc
  set {  // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  }
}

var subject_global_observingAccesorsVar1: Int = 0 {
  @objc 
  willSet { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  }
  @objc 
  didSet { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  }
}

class subject_getterSetter1 {
  var instanceVar1: Int {
    @objc
    get { // expected-error {{'@objc' getter for non-'@objc' property}}
      return 0
    }
  }

  var instanceVar2: Int {
    get {
      return 0
    }
    @objc
    set { // expected-error {{'@objc' setter for non-'@objc' property}}
    }
  }

  var instanceVar3: Int {
    @objc
    get { // expected-error {{'@objc' getter for non-'@objc' property}}
      return 0
    }
    @objc
    set { // expected-error {{'@objc' setter for non-'@objc' property}}
    }
  }

  var observingAccesorsVar1: Int = 0 {
    @objc
    willSet { // expected-error {{observing accessors are not allowed to be marked @objc}}
    }
    @objc
    didSet { // expected-error {{observing accessors are not allowed to be marked @objc}}
    }
  }
}

class subject_staticVar1 {
  @objc
  class var staticVar1: Int = 42 // expected-error {{class stored properties not yet supported}}

  @objc
  class var staticVar2: Int { return 42 }
}

@objc
func subject_freeFunc() { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  @objc
  var subject_localVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  func subject_nestedFreeFunc() { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  }
}

@objc
func subject_genericFunc<T>(t: T) { // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
  @objc
  var subject_localVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
}

func subject_funcParam(a: @objc Int) { // expected-error {{attribute can only be applied to declarations, not types}}
}

@objc // expected-error {{@objc cannot be applied to this declaration}}
struct subject_struct {
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
}

@objc   // expected-error {{@objc cannot be applied to this declaration}}
struct subject_genericStruct<T> {
  @objc
  var subject_instanceVar: Int // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
}

@objc
class subject_class1 { // no-error
  @objc
  var subject_instanceVar: Int // no-error

  @objc
  init() {} // no-error

  @objc
  func subject_instanceFunc() {} // no-error
}

@objc
class subject_class2 : Protocol_Class1, PlainProtocol { // no-error
}

@objc
class subject_genericClass<T> { // no-error
  @objc
  var subject_instanceVar: Int // expected-error{{variable in a generic class cannot be represented in Objective-C}}

  @objc
  init() {} // expected-error{{initializer in a generic class cannot be represented in Objective-C}}

  @objc
  func subject_instanceFunc() {} // expected-error{{method in a generic class cannot be represented in Objective-C}}
}

@objc
enum subject_enum: Int {
  @objc   // expected-error {{@objc cannot be applied to this declaration}}
  case subject_enumElement1

  @objc   
  init() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes, protocols, methods, properties, and subscript declarations can be declared @objc}}
}

@objc
protocol subject_protocol1 {
  @objc
  var subject_instanceVar: Int { get }

  @objc
  func subject_instanceFunc()
}

@objc protocol subject_protocol2 {} // no-error
// CHECK-LABEL: @objc protocol subject_protocol2 {

@objc protocol subject_protocol3 {} // no-error
// CHECK-LABEL: @objc protocol subject_protocol3 {

@objc
protocol subject_protocol4 : PlainProtocol {} // expected-error {{@objc protocol 'subject_protocol4' cannot refine non-@objc protocol 'PlainProtocol'}}

@objc
protocol subject_protocol5 : Protocol_Class1 {} // expected-error {{@objc protocol 'subject_protocol5' cannot refine non-@objc protocol 'Protocol_Class1'}}

@objc
protocol subject_protocol6 : Protocol_ObjC1 {}

protocol subject_containerProtocol1 {
  @objc
  var subject_instanceVar: Int { get }

  @objc
  func subject_instanceFunc()

  @objc
  static func subject_staticFunc()
}

@objc
protocol subject_containerObjCProtocol1 {
  func func_Curried1()()
  // expected-error@-1 {{method cannot be a member of an @objc protocol because curried functions cannot be represented in Objective-C}}
  // expected-note@-2 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  func func_FunctionReturn1() -> PlainStruct
  // expected-error@-1 {{method cannot be a member of an @objc protocol because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-note@-3 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  func func_FunctionParam1(a: PlainStruct)
  // expected-error@-1 {{method cannot be a member of an @objc protocol because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-note@-3 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  func func_Variadic(AnyObject...)
  // expected-error @-1{{method cannot be a member of an @objc protocol because it has a variadic parameter}}
  // expected-note @-2{{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  subscript(a: PlainStruct) -> Int { get }
  // expected-error@-1 {{subscript cannot be a member of an @objc protocol because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-note@-3 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  var varNonObjC1: PlainStruct { get }
  // expected-error@-1 {{property cannot be a member of an @objc protocol because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-note@-3 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}
}

@objc
protocol subject_containerObjCProtocol2 {
  init(a: Int)
  @objc init(a: Double)

  func func1() -> Int
  @objc func func1_() -> Int

  var instanceVar1: Int { get set }
  @objc var instanceVar1_: Int { get set }

  subscript(i: Int) -> Int { get set }
  @objc subscript(i: String) -> Int { get set}
}

func concreteContext1() {
  @objc
  class subject_inConcreteContext {}
}

class ConcreteContext2 {
  @objc
  class subject_inConcreteContext {}
}

class ConcreteContext3 {
  func dynamicSelf1() -> Self { return self }

  @objc func dynamicSelf1_() -> Self { return self }
  // expected-error@-1{{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
}

func genericContext1<T>(_: T) {
  @objc
  class subject_inGenericContext {} // expected-error{{type 'subject_inGenericContext' nested in generic function 'genericContext1' is not allowed}}

  class subject_constructor_inGenericContext { // expected-error{{type 'subject_constructor_inGenericContext' nested in generic function 'genericContext1' is not allowed}}
    @objc
    init() {} // expected-error{{initializer in a generic class cannot be represented in Objective-C}}
  }

  class subject_var_inGenericContext { // expected-error{{type 'subject_var_inGenericContext' nested in generic function 'genericContext1' is not allowed}}
    @objc
    var subject_instanceVar: Int = 0 // expected-error{{variable in a generic class cannot be represented in Objective-C}}
  }

  class subject_func_inGenericContext { // expected-error{{type 'subject_func_inGenericContext' nested in generic function 'genericContext1' is not allowed}}
    @objc
    func f() {} // expected-error{{method in a generic class cannot be represented in Objective-C}}
  }
}

class GenericContext2<T> {
  @objc
  class subject_inGenericContext {} // expected-error{{nested in generic type}}

  @objc
  func f() {} // expected-error{{method in a generic class cannot be represented in Objective-C}}
}

class GenericContext3<T> {
  class MoreNested { // expected-error{{nested in generic type}}
    @objc
    class subject_inGenericContext {} // expected-error{{nested in generic type}}

    @objc
    func f() {} // expected-error{{method in a generic class cannot be represented in Objective-C}}
  }
}


class subject_subscriptIndexed1 {
  @objc
  subscript(a: Int) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptIndexed2 {
  @objc
  subscript(a: Int8) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptIndexed3 {
  @objc
  subscript(a: UInt8) -> Int { // no-error
    get { return 0 }
  }
}

class subject_subscriptKeyed1 {
  @objc
  subscript(a: String) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed2 {
  @objc
  subscript(a: Class_ObjC1) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed3 {
  @objc
  subscript(a: Class_ObjC1.Type) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed4 {
  @objc
  subscript(a: Protocol_ObjC1) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed5 {
  @objc
  subscript(a: Protocol_ObjC1.Type) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed6 {
  @objc
  subscript(a: protocol<Protocol_ObjC1, Protocol_ObjC2>) -> Int { // no-error
    get { return 0 }
  }
}
class subject_subscriptKeyed7 {
  @objc
  subscript(a: protocol<Protocol_ObjC1, Protocol_ObjC2>.Type) -> Int { // no-error
    get { return 0 }
  }
}



class subject_subscriptBridgedFloat {
  @objc
  subscript(a: Float32) -> Int {
    get { return 0 }
  }
}
class subject_subscriptInvalid2 {
  @objc
  subscript(a: PlainClass) -> Int {
  // expected-error@-1 {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{classes not annotated with @objc cannot be represented in Objective-C}}
    get { return 0 }
  }
}
class subject_subscriptInvalid3 {
  @objc
  subscript(a: PlainClass.Type) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    get { return 0 }
  }
}
class subject_subscriptInvalid4 {
  @objc
  subscript(a: PlainStruct) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    // expected-note@-1{{Swift structs cannot be represented in Objective-C}}
    get { return 0 }
  }
}
class subject_subscriptInvalid5 {
  @objc
  subscript(a: PlainEnum) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    // expected-note@-1{{enums cannot be represented in Objective-C}}
    get { return 0 }
  }
}
class subject_subscriptInvalid6 {
  @objc
  subscript(a: PlainProtocol) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    // expected-note@-1{{protocol 'PlainProtocol' is not '@objc'}}
    get { return 0 }
  }
}
class subject_subscriptInvalid7 {
  @objc
  subscript(a: Protocol_Class1) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    // expected-note@-1{{protocol 'Protocol_Class1' is not '@objc'}}
    get { return 0 }
  }
}
class subject_subscriptInvalid8 {
  @objc
  subscript(a: protocol<Protocol_Class1, Protocol_Class2>) -> Int { // expected-error {{subscript cannot be marked @objc because its type cannot be represented in Objective-C}}
    // expected-note@-1{{protocol 'Protocol_Class1' is not '@objc'}}
    get { return 0 }
  }
}
class subject_subscriptInvalid9<T> {
  @objc
  subscript(a: Int) -> Int { // expected-error{{subscript in a generic class cannot be represented in Objective-C}}
    get { return 0 }
  }
}

//===--- Tests for @objc inference.

@objc
class infer_instanceFunc1 {
// CHECK-LABEL: @objc class infer_instanceFunc1 {

  func func1() {}
// CHECK-LABEL: @objc func func1() {

  @objc func func1_() {} // no-error

  func func2(a: Int) {}
// CHECK-LABEL: @objc func func2(a: Int) {

  @objc func func2_(a: Int) {} // no-error

  func func3(a: Int) -> Int {}
// CHECK-LABEL: @objc func func3(a: Int) -> Int {

  @objc func func3_(a: Int) -> Int {} // no-error

  func func4(a: Int, b: Double) {}
// CHECK-LABEL: @objc func func4(a: Int, b: Double) {

  @objc func func4_(a: Int, b: Double) {} // no-error

  func func5(a: String) {}
// CHECK-LABEL: @objc func func5(a: String) {

  @objc func func5_(a: String) {} // no-error

  func func6() -> String {}
// CHECK-LABEL: @objc func func6() -> String {

  @objc func func6_() -> String {} // no-error

  func func7(a: PlainClass) {}
// CHECK-LABEL: {{^}} func func7(a: PlainClass) {

  @objc func func7_(a: PlainClass) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{classes not annotated with @objc cannot be represented in Objective-C}}

  func func7m(a: PlainClass.Type) {}
// CHECK-LABEL: {{^}} func func7m(a: PlainClass.Type) {

  @objc func func7m_(a: PlainClass.Type) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}

  func func8() -> PlainClass {}
// CHECK-LABEL: {{^}} func func8() -> PlainClass {

  @objc func func8_() -> PlainClass {}
  // expected-error@-1 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{classes not annotated with @objc cannot be represented in Objective-C}}

  func func8m() -> PlainClass.Type {}
// CHECK-LABEL: {{^}} func func8m() -> PlainClass.Type {

  @objc func func8m_() -> PlainClass.Type {}
  // expected-error@-1 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}

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
  // expected-note@-2 {{non-'@objc' enums cannot be represented in Objective-C}}

  func func12(a: PlainProtocol) {}
// CHECK-LABEL: {{^}} func func12(a: PlainProtocol) {

  @objc func func12_(a: PlainProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  func func13(a: Class_ObjC1) {}
// CHECK-LABEL: @objc func func13(a: Class_ObjC1) {

  @objc func func13_(a: Class_ObjC1) {} // no-error

  func func14(a: Protocol_Class1) {}
// CHECK-LABEL: {{^}} func func14(a: Protocol_Class1) {

  @objc func func14_(a: Protocol_Class1) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  func func15(a: Protocol_ObjC1) {}
// CHECK-LABEL: @objc func func15(a: Protocol_ObjC1) {
  @objc func func15_(a: Protocol_ObjC1) {} // no-error

  func func16(a: AnyObject) {}
// CHECK-LABEL: @objc func func16(a: AnyObject) {

  @objc func func16_(a: AnyObject) {} // no-error

  func func17(a: () -> ()) {}
// CHECK-LABEL: {{^}}  @objc func func17(a: () -> ()) {

  @objc func func17_(a: () -> ()) {}

  func func18(a: (Int) -> (), b: Int) {}
// CHECK-LABEL: {{^}}  @objc func func18(a: (Int) -> (), b: Int)

  @objc func func18_(a: (Int) -> (), b: Int) {}

  func func19(a: (String) -> (), b: Int) {}
// CHECK-LABEL: {{^}}  @objc func func19(a: (String) -> (), b: Int) {

  @objc func func19_(a: (String) -> (), b: Int) {}

  func func_FunctionReturn1() -> () -> () {}
// CHECK-LABEL: {{^}}  @objc func func_FunctionReturn1() -> () -> () {

  @objc func func_FunctionReturn1_() -> () -> () {}

  func func_FunctionReturn2() -> (Int) -> () {}
// CHECK-LABEL: {{^}}  @objc func func_FunctionReturn2() -> (Int) -> () {

  @objc func func_FunctionReturn2_() -> (Int) -> () {}

  func func_FunctionReturn3() -> () -> Int {}
// CHECK-LABEL: {{^}}  @objc func func_FunctionReturn3() -> () -> Int {

  @objc func func_FunctionReturn3_() -> () -> Int {}

  func func_FunctionReturn4() -> (String) -> () {}
// CHECK-LABEL: {{^}}  @objc func func_FunctionReturn4() -> (String) -> () {

  @objc func func_FunctionReturn4_() -> (String) -> () {}

  func func_FunctionReturn5() -> () -> String {}
// CHECK-LABEL: {{^}}  @objc func func_FunctionReturn5() -> () -> String {

  @objc func func_FunctionReturn5_() -> () -> String {}


  func func_ZeroParams1() {}
// CHECK-LABEL: @objc func func_ZeroParams1() {

  @objc func func_ZeroParams1a() {} // no-error


  func func_OneParam1(a: Int) {}
// CHECK-LABEL: @objc func func_OneParam1(a: Int) {

  @objc func func_OneParam1a(a: Int) {} // no-error


  func func_TupleStyle1(a: Int, b: Int) {}
  // CHECK-LABEL: {{^}} @objc func func_TupleStyle1(a: Int, b: Int) {

  @objc func func_TupleStyle1a(a: Int, b: Int) {}

  func func_TupleStyle2(a: Int, b: Int, c: Int) {}
// CHECK-LABEL: {{^}} @objc func func_TupleStyle2(a: Int, b: Int, c: Int) {

  @objc func func_TupleStyle2a(a: Int, b: Int, c: Int) {}

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
  @objc func func_MultipleDiags(a: PlainStruct, b: PlainEnum) -> protocol<> {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter 1 cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // expected-error@-3 {{method cannot be marked @objc because the type of the parameter 2 cannot be represented in Objective-C}}
  // expected-note@-4 {{non-'@objc' enums cannot be represented in Objective-C}}
  // expected-error@-5 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-6 {{'protocol<>' is not considered '@objc'; use 'AnyObject' instead}}

  @objc func func_UnnamedParam1(_: Int) {} // no-error

  @objc func func_UnnamedParam2(_: PlainStruct) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  @objc func func_varParam1(var a: AnyObject) {
    let b = a; a = b
  }

  func func_varParam2(var a: AnyObject) {
    let b = a; a = b
  }
// CHECK-LABEL: @objc func func_varParam2(a: AnyObject) {
}

@objc
class infer_constructor1 {
// CHECK-LABEL: @objc class infer_constructor1

  init() {}
  // CHECK: @objc init()

  init(a: Int) {}
  // CHECK: @objc init(a: Int)

  init(a: PlainStruct) {}
  // CHECK: {{^}} init(a: PlainStruct)

  init(malice: ()) {}
  // CHECK: @objc init(malice: ())

  init(forMurder _: ()) {}
  // CHECK: @objc init(forMurder _: ())
}

@objc
class infer_destructor1 {
// CHECK-LABEL: @objc class infer_destructor1

  deinit {}
  // CHECK: @objc deinit
}

// @!objc
class infer_destructor2 {
// CHECK-LABEL: {{^}}class infer_destructor2

  deinit {}
  // CHECK: @objc deinit
}

@objc
class infer_instanceVar1 {
// CHECK-LABEL: @objc class infer_instanceVar1 {
  init() {}

  var instanceVar1: Int
  // CHECK: @objc var instanceVar1: Int

  var (instanceVar2, instanceVar3): (Int, PlainProtocol)
  // CHECK: @objc var instanceVar2: Int
  // CHECK: {{^}}  var instanceVar3: PlainProtocol

  @objc var (instanceVar1_, instanceVar2_): (Int, PlainProtocol)
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var intstanceVar4: Int {
  // CHECK: @objc var intstanceVar4: Int {
    get {}
    // CHECK-NEXT: @objc get {}
  }

  var intstanceVar5: Int {
  // CHECK: @objc var intstanceVar5: Int {
    get {}
    // CHECK-NEXT: @objc get {}
    set {}
    // CHECK-NEXT: @objc set {}
  }

  @objc var instanceVar5_: Int {
  // CHECK: @objc var instanceVar5_: Int {
    get {}
    // CHECK-NEXT: @objc get {}
    set {}
    // CHECK-NEXT: @objc set {}
  }

  var observingAccesorsVar1: Int {
  // CHECK: @objc var observingAccesorsVar1: Int {
    willSet {}
    // CHECK-NEXT: {{^}} final willSet {}
    didSet {}
    // CHECK-NEXT: {{^}} final didSet {}
  }

  @objc var observingAccesorsVar1_: Int {
  // CHECK: {{^}} @objc var observingAccesorsVar1_: Int {
    willSet {}
    // CHECK-NEXT: {{^}} final willSet {}
    didSet {}
    // CHECK-NEXT: {{^}} final didSet {}
  }


  var var_Int: Int
// CHECK-LABEL: @objc var var_Int: Int

  var var_Bool: Bool
// CHECK-LABEL: @objc var var_Bool: Bool

  var var_CBool: CBool
// CHECK-LABEL: @objc var var_CBool: CBool

  var var_String: String
// CHECK-LABEL: @objc var var_String: String

  var var_Float: Float
  var var_Double: Double
// CHECK-LABEL: @objc var var_Float: Float
// CHECK-LABEL: @objc var var_Double: Double

  var var_Char: UnicodeScalar
// CHECK-LABEL: @objc var var_Char: UnicodeScalar

  //===--- Tuples.

  var var_tuple1: ()
// CHECK-LABEL: {{^}} var var_tuple1: ()

  @objc var var_tuple1_: ()
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{empty tuple type cannot be represented in Objective-C}}

  var var_tuple2: Void
// CHECK-LABEL: {{^}} var var_tuple2: Void

  @objc var var_tuple2_: Void
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{empty tuple type cannot be represented in Objective-C}}

  var var_tuple3: (Int)
// CHECK-LABEL: @objc var var_tuple3: (Int)

  @objc var var_tuple3_: (Int) // no-error

  var var_tuple4: (Int, Int)
// CHECK-LABEL: {{^}} var var_tuple4: (Int, Int)

  @objc var var_tuple4_: (Int, Int)
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{tuples cannot be represented in Objective-C}}

  //===--- Stdlib integer types.

  var var_Int8: Int8
  var var_Int16: Int16
  var var_Int32: Int32
  var var_Int64: Int64
// CHECK-LABEL: @objc var var_Int8: Int8
// CHECK-LABEL: @objc var var_Int16: Int16
// CHECK-LABEL: @objc var var_Int32: Int32
// CHECK-LABEL: @objc var var_Int64: Int64

  var var_UInt8: UInt8
  var var_UInt16: UInt16
  var var_UInt32: UInt32
  var var_UInt64: UInt64
// CHECK-LABEL: @objc var var_UInt8: UInt8
// CHECK-LABEL: @objc var var_UInt16: UInt16
// CHECK-LABEL: @objc var var_UInt32: UInt32
// CHECK-LABEL: @objc var var_UInt64: UInt64

  var var_COpaquePointer: COpaquePointer
// CHECK-LABEL: @objc var var_COpaquePointer: COpaquePointer

  var var_PlainClass: PlainClass
// CHECK-LABEL: {{^}}  var var_PlainClass: PlainClass

  @objc var var_PlainClass_: PlainClass
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{classes not annotated with @objc cannot be represented in Objective-C}}

  var var_PlainStruct: PlainStruct
// CHECK-LABEL: {{^}}  var var_PlainStruct: PlainStruct

  @objc var var_PlainStruct_: PlainStruct
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  var var_PlainEnum: PlainEnum
// CHECK-LABEL: {{^}}  var var_PlainEnum: PlainEnum

  @objc var var_PlainEnum_: PlainEnum
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{non-'@objc' enums cannot be represented in Objective-C}}

  var var_PlainProtocol: PlainProtocol
// CHECK-LABEL: {{^}}  var var_PlainProtocol: PlainProtocol

  @objc var var_PlainProtocol_: PlainProtocol
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_ClassObjC: Class_ObjC1
// CHECK-LABEL: @objc var var_ClassObjC: Class_ObjC1

  @objc var var_ClassObjC_: Class_ObjC1 // no-error

  var var_ProtocolClass: Protocol_Class1
// CHECK-LABEL: {{^}}  var var_ProtocolClass: Protocol_Class1

  @objc var var_ProtocolClass_: Protocol_Class1
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_ProtocolObjC: Protocol_ObjC1
// CHECK-LABEL: @objc var var_ProtocolObjC: Protocol_ObjC1

  @objc var var_ProtocolObjC_: Protocol_ObjC1 // no-error


  var var_PlainClassMetatype: PlainClass.Type
// CHECK-LABEL: {{^}}  var var_PlainClassMetatype: PlainClass.Type

  @objc var var_PlainClassMetatype_: PlainClass.Type
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_PlainStructMetatype: PlainStruct.Type
// CHECK-LABEL: {{^}}  var var_PlainStructMetatype: PlainStruct.Type

  @objc var var_PlainStructMetatype_: PlainStruct.Type
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_PlainEnumMetatype: PlainEnum.Type
// CHECK-LABEL: {{^}}  var var_PlainEnumMetatype: PlainEnum.Type

  @objc var var_PlainEnumMetatype_: PlainEnum.Type
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_PlainExistentialMetatype: PlainProtocol.Type
// CHECK-LABEL: {{^}}  var var_PlainExistentialMetatype: PlainProtocol.Type

  @objc var var_PlainExistentialMetatype_: PlainProtocol.Type
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_ClassObjCMetatype: Class_ObjC1.Type
// CHECK-LABEL: @objc var var_ClassObjCMetatype: Class_ObjC1.Type

  @objc var var_ClassObjCMetatype_: Class_ObjC1.Type // no-error

  var var_ProtocolClassMetatype: Protocol_Class1.Type
// CHECK-LABEL: {{^}}  var var_ProtocolClassMetatype: Protocol_Class1.Type

  @objc var var_ProtocolClassMetatype_: Protocol_Class1.Type
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_ProtocolObjCMetatype1: Protocol_ObjC1.Type
// CHECK-LABEL: @objc var var_ProtocolObjCMetatype1: Protocol_ObjC1.Type

  @objc var var_ProtocolObjCMetatype1_: Protocol_ObjC1.Type // no-error

  var var_ProtocolObjCMetatype2: Protocol_ObjC2.Type
// CHECK-LABEL: @objc var var_ProtocolObjCMetatype2: Protocol_ObjC2.Type

  @objc var var_ProtocolObjCMetatype2_: Protocol_ObjC2.Type // no-error

  var var_AnyObject1: AnyObject
  var var_AnyObject2: AnyObject.Type
// CHECK-LABEL: @objc var var_AnyObject1: AnyObject
// CHECK-LABEL: @objc var var_AnyObject2: AnyObject.Type

  var var_Existential0: protocol<>
// CHECK-LABEL: {{^}}  var var_Existential0: protocol<>

  @objc var var_Existential0_: protocol<>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{'protocol<>' is not considered '@objc'; use 'AnyObject' instead}}

  var var_Existential1: protocol<PlainProtocol>
// CHECK-LABEL: {{^}}  var var_Existential1: PlainProtocol

  @objc var var_Existential1_: protocol<PlainProtocol>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential2: protocol<PlainProtocol, PlainProtocol>
// CHECK-LABEL: {{^}}  var var_Existential2: PlainProtocol

  @objc var var_Existential2_: protocol<PlainProtocol, PlainProtocol>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential3: protocol<PlainProtocol, Protocol_Class1>
// CHECK-LABEL: {{^}}  var var_Existential3: protocol<PlainProtocol, Protocol_Class1>

  @objc var var_Existential3_: protocol<PlainProtocol, Protocol_Class1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential4: protocol<PlainProtocol, Protocol_ObjC1>
// CHECK-LABEL: {{^}}  var var_Existential4: protocol<PlainProtocol, Protocol_ObjC1>

  @objc var var_Existential4_: protocol<PlainProtocol, Protocol_ObjC1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'PlainProtocol' is not '@objc'}}

  var var_Existential5: protocol<Protocol_Class1>
// CHECK-LABEL: {{^}}  var var_Existential5: Protocol_Class1

  @objc var var_Existential5_: protocol<Protocol_Class1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential6: protocol<Protocol_Class1, Protocol_Class2>
// CHECK-LABEL: {{^}}  var var_Existential6: protocol<Protocol_Class1, Protocol_Class2>

  @objc var var_Existential6_: protocol<Protocol_Class1, Protocol_Class2>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential7: protocol<Protocol_Class1, Protocol_ObjC1>
// CHECK-LABEL: {{^}}  var var_Existential7: protocol<Protocol_Class1, Protocol_ObjC1>

  @objc var var_Existential7_: protocol<Protocol_Class1, Protocol_ObjC1>
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol 'Protocol_Class1' is not '@objc'}}

  var var_Existential8: protocol<Protocol_ObjC1>
// CHECK-LABEL: @objc var var_Existential8: Protocol_ObjC1

  @objc var var_Existential8_: protocol<Protocol_ObjC1> // no-error

  var var_Existential9: protocol<Protocol_ObjC1, Protocol_ObjC2>
// CHECK-LABEL: @objc var var_Existential9: protocol<Protocol_ObjC1, Protocol_ObjC2>

  @objc var var_Existential9_: protocol<Protocol_ObjC1, Protocol_ObjC2> // no-error


  var var_ExistentialMetatype0: protocol<>.Type
  var var_ExistentialMetatype1: protocol<PlainProtocol>.Type
  var var_ExistentialMetatype2: protocol<PlainProtocol, PlainProtocol>.Type
  var var_ExistentialMetatype3: protocol<PlainProtocol, Protocol_Class1>.Type
  var var_ExistentialMetatype4: protocol<PlainProtocol, Protocol_ObjC1>.Type
  var var_ExistentialMetatype5: protocol<Protocol_Class1>.Type
  var var_ExistentialMetatype6: protocol<Protocol_Class1, Protocol_Class2>.Type
  var var_ExistentialMetatype7: protocol<Protocol_Class1, Protocol_ObjC1>.Type
  var var_ExistentialMetatype8: protocol<Protocol_ObjC1>.Type
  var var_ExistentialMetatype9: protocol<Protocol_ObjC1, Protocol_ObjC2>.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype0: protocol<>.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype1: PlainProtocol.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype2: PlainProtocol.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype3: protocol<PlainProtocol, Protocol_Class1>.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype4: protocol<PlainProtocol, Protocol_ObjC1>.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype5: Protocol_Class1.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype6: protocol<Protocol_Class1, Protocol_Class2>.Type
// CHECK-LABEL: {{^}}  var var_ExistentialMetatype7: protocol<Protocol_Class1, Protocol_ObjC1>.Type
// CHECK-LABEL: @objc var var_ExistentialMetatype8: Protocol_ObjC1.Type
// CHECK-LABEL: @objc var var_ExistentialMetatype9: protocol<Protocol_ObjC1, Protocol_ObjC2>.Type


  var var_UnsafeMutablePointer1: UnsafeMutablePointer<Int>
  var var_UnsafeMutablePointer2: UnsafeMutablePointer<Bool>
  var var_UnsafeMutablePointer3: UnsafeMutablePointer<CBool>
  var var_UnsafeMutablePointer4: UnsafeMutablePointer<String>
  var var_UnsafeMutablePointer5: UnsafeMutablePointer<Float>
  var var_UnsafeMutablePointer6: UnsafeMutablePointer<Double>
  var var_UnsafeMutablePointer7: UnsafeMutablePointer<COpaquePointer>
  var var_UnsafeMutablePointer8: UnsafeMutablePointer<PlainClass>
  var var_UnsafeMutablePointer9: UnsafeMutablePointer<PlainStruct>
  var var_UnsafeMutablePointer10: UnsafeMutablePointer<PlainEnum>
  var var_UnsafeMutablePointer11: UnsafeMutablePointer<PlainProtocol>
  var var_UnsafeMutablePointer12: UnsafeMutablePointer<AnyObject>
  var var_UnsafeMutablePointer13: UnsafeMutablePointer<AnyObject.Type>
  var var_UnsafeMutablePointer100: UnsafeMutablePointer<()>
  var var_UnsafeMutablePointer101: UnsafeMutablePointer<Void>
  var var_UnsafeMutablePointer102: UnsafeMutablePointer<(Int, Int)>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer1: UnsafeMutablePointer<Int>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer2: UnsafeMutablePointer<Bool>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer3: UnsafeMutablePointer<CBool>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer4: UnsafeMutablePointer<String>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer5: UnsafeMutablePointer<Float>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer6: UnsafeMutablePointer<Double>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer7: UnsafeMutablePointer<COpaquePointer>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer8: UnsafeMutablePointer<PlainClass>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer9: UnsafeMutablePointer<PlainStruct>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer10: UnsafeMutablePointer<PlainEnum>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer11: UnsafeMutablePointer<PlainProtocol>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer12: UnsafeMutablePointer<AnyObject>
// CHECK-LABEL: @objc var var_UnsafeMutablePointer13: UnsafeMutablePointer<AnyObject.Type>
// CHECK-LABEL: {{^}} @objc var var_UnsafeMutablePointer100: UnsafeMutablePointer<()>
// CHECK-LABEL: {{^}} @objc var var_UnsafeMutablePointer101: UnsafeMutablePointer<Void>
// CHECK-LABEL: {{^}}  var var_UnsafeMutablePointer102: UnsafeMutablePointer<(Int, Int)>

  var var_Optional1: Class_ObjC1?
  var var_Optional2: Protocol_ObjC1?
  var var_Optional3: Class_ObjC1.Type?
  var var_Optional4: Protocol_ObjC1.Type?
  var var_Optional5: AnyObject?
  var var_Optional6: AnyObject.Type?
  var var_Optional7: String?
  var var_Optional8: protocol<Protocol_ObjC1>?
  var var_Optional9: protocol<Protocol_ObjC1>.Type?
  var var_Optional10: protocol<Protocol_ObjC1, Protocol_ObjC2>?
  var var_Optional11: protocol<Protocol_ObjC1, Protocol_ObjC2>.Type?

// CHECK-LABEL: @objc var var_Optional1: Class_ObjC1?
// CHECK-LABEL: @objc var var_Optional2: Protocol_ObjC1?
// CHECK-LABEL: @objc var var_Optional3: Class_ObjC1.Type?
// CHECK-LABEL: @objc var var_Optional4: Protocol_ObjC1.Type?
// CHECK-LABEL: @objc var var_Optional5: AnyObject?
// CHECK-LABEL: @objc var var_Optional6: AnyObject.Type?
// CHECK-LABEL: @objc var var_Optional7: String?
// CHECK-LABEL: @objc var var_Optional8: Protocol_ObjC1?
// CHECK-LABEL: @objc var var_Optional9: Protocol_ObjC1.Type?
// CHECK-LABEL: @objc var var_Optional10: protocol<Protocol_ObjC1, Protocol_ObjC2>?
// CHECK-LABEL: @objc var var_Optional11: protocol<Protocol_ObjC1, Protocol_ObjC2>.Type?


  var var_ImplicitlyUnwrappedOptional1: Class_ObjC1!
  var var_ImplicitlyUnwrappedOptional2: Protocol_ObjC1!
  var var_ImplicitlyUnwrappedOptional3: Class_ObjC1.Type!
  var var_ImplicitlyUnwrappedOptional4: Protocol_ObjC1.Type!
  var var_ImplicitlyUnwrappedOptional5: AnyObject!
  var var_ImplicitlyUnwrappedOptional6: AnyObject.Type!
  var var_ImplicitlyUnwrappedOptional7: String!
  var var_ImplicitlyUnwrappedOptional8: protocol<Protocol_ObjC1>!
  var var_ImplicitlyUnwrappedOptional9: protocol<Protocol_ObjC1, Protocol_ObjC2>!

// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional1: Class_ObjC1!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional2: Protocol_ObjC1!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional3: Class_ObjC1.Type!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional4: Protocol_ObjC1.Type!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional5: AnyObject!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional6: AnyObject.Type!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional7: String!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional8: Protocol_ObjC1!
// CHECK-LABEL: @objc var var_ImplicitlyUnwrappedOptional9: protocol<Protocol_ObjC1, Protocol_ObjC2>!

  var var_Optional_fail1: PlainClass?
  var var_Optional_fail2: PlainClass.Type?
  var var_Optional_fail3: PlainClass!
  var var_Optional_fail4: PlainStruct?
  var var_Optional_fail5: PlainStruct.Type?
  var var_Optional_fail6: PlainEnum?
  var var_Optional_fail7: PlainEnum.Type?
  var var_Optional_fail8: PlainProtocol?
  var var_Optional_fail9: protocol<>?
  var var_Optional_fail10: protocol<PlainProtocol>?
  var var_Optional_fail11: protocol<PlainProtocol, Protocol_ObjC1>?
  var var_Optional_fail12: Int?
  var var_Optional_fail13: Bool?
  var var_Optional_fail14: CBool?
  var var_Optional_fail16: COpaquePointer?
  var var_Optional_fail17: UnsafeMutablePointer<Int>?
  var var_Optional_fail18: UnsafeMutablePointer<Class_ObjC1>?
  var var_Optional_fail20: AnyObject??
  var var_Optional_fail21: AnyObject.Type??
// CHECK-NOT: @objc{{.*}}Optional_fail

  // CHECK-LABEL: @objc var var_CFunctionPointer_1: @convention(c) () -> ()
  var var_CFunctionPointer_1: @convention(c) () -> ()
  // CHECK-LABEL: @objc var var_CFunctionPointer_invalid_1: Int
  var var_CFunctionPointer_invalid_1: @convention(c) Int // expected-error {{attribute only applies to syntactic function types}}
  // CHECK-LABEL: {{^}} var var_CFunctionPointer_invalid_2: @convention(c) PlainStruct -> Int
  var var_CFunctionPointer_invalid_2: @convention(c) PlainStruct -> Int // expected-error {{@convention(c) type is not representable in Objective-C}}

  weak var var_Weak1: Class_ObjC1?
  weak var var_Weak2: Protocol_ObjC1?
  // <rdar://problem/16473062> weak and unowned variables of metatypes are rejected
  //weak var var_Weak3: Class_ObjC1.Type?
  //weak var var_Weak4: Protocol_ObjC1.Type?
  weak var var_Weak5: AnyObject?
  //weak var var_Weak6: AnyObject.Type?
  weak var var_Weak7: protocol<Protocol_ObjC1>?
  weak var var_Weak8: protocol<Protocol_ObjC1, Protocol_ObjC2>?

// CHECK-LABEL: @objc weak var var_Weak1: @sil_weak Class_ObjC1
// CHECK-LABEL: @objc weak var var_Weak2: @sil_weak Protocol_ObjC1
// CHECK-LABEL: @objc weak var var_Weak5: @sil_weak AnyObject
// CHECK-LABEL: @objc weak var var_Weak7: @sil_weak Protocol_ObjC1
// CHECK-LABEL: @objc weak var var_Weak8: @sil_weak protocol<Protocol_ObjC1, Protocol_ObjC2>


  weak var var_Weak_fail1: PlainClass?
  weak var var_Weak_bad2: PlainStruct?
  // expected-error@-1 {{'weak' cannot be applied to non-class type 'PlainStruct'}}
  weak var var_Weak_bad3: PlainEnum?
  // expected-error@-1 {{'weak' cannot be applied to non-class type 'PlainEnum'}}
  weak var var_Weak_bad4: String?
  // expected-error@-1 {{'weak' cannot be applied to non-class type 'String'}}
// CHECK-NOT: @objc{{.*}}Weak_fail


  unowned var var_Unowned1: Class_ObjC1
  unowned var var_Unowned2: Protocol_ObjC1
  // <rdar://problem/16473062> weak and unowned variables of metatypes are rejected
  //unowned var var_Unowned3: Class_ObjC1.Type
  //unowned var var_Unowned4: Protocol_ObjC1.Type
  unowned var var_Unowned5: AnyObject
  //unowned var var_Unowned6: AnyObject.Type
  unowned var var_Unowned7: protocol<Protocol_ObjC1>
  unowned var var_Unowned8: protocol<Protocol_ObjC1, Protocol_ObjC2>

// CHECK-LABEL: @objc unowned var var_Unowned1: @sil_unowned Class_ObjC1
// CHECK-LABEL: @objc unowned var var_Unowned2: @sil_unowned Protocol_ObjC1
// CHECK-LABEL: @objc unowned var var_Unowned5: @sil_unowned AnyObject
// CHECK-LABEL: @objc unowned var var_Unowned7: @sil_unowned Protocol_ObjC1
// CHECK-LABEL: @objc unowned var var_Unowned8: @sil_unowned protocol<Protocol_ObjC1, Protocol_ObjC2>


  unowned var var_Unowned_fail1: PlainClass
  unowned var var_Unowned_bad2: PlainStruct
  // expected-error@-1 {{'unowned' cannot be applied to non-class type 'PlainStruct'}}
  unowned var var_Unowned_bad3: PlainEnum
  // expected-error@-1 {{'unowned' cannot be applied to non-class type 'PlainEnum'}}
  unowned var var_Unowned_bad4: String
  // expected-error@-1 {{'unowned' cannot be applied to non-class type 'String'}}
// CHECK-NOT: @objc{{.*}}Unowned_fail


  var var_FunctionType1: () -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType1: () -> ()

  var var_FunctionType2: (Int) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType2: (Int) -> ()

  var var_FunctionType3: (Int) -> Int
// CHECK-LABEL: {{^}}  @objc var var_FunctionType3: (Int) -> Int

  var var_FunctionType4: (Int, Double) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType4: (Int, Double) -> ()

  var var_FunctionType5: (String) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType5: (String) -> ()

  var var_FunctionType6: () -> String
// CHECK-LABEL: {{^}}  @objc var var_FunctionType6: () -> String

  var var_FunctionType7: (PlainClass) -> ()
// CHECK-NOT: @objc var var_FunctionType7: (PlainClass) -> ()

  var var_FunctionType8: () -> PlainClass
// CHECK-NOT: @objc var var_FunctionType8: () -> PlainClass

  var var_FunctionType9: (PlainStruct) -> ()
// CHECK-LABEL: {{^}}  var var_FunctionType9: (PlainStruct) -> ()

  var var_FunctionType10: () -> PlainStruct
// CHECK-LABEL: {{^}}  var var_FunctionType10: () -> PlainStruct

  var var_FunctionType11: (PlainEnum) -> ()
// CHECK-LABEL: {{^}}  var var_FunctionType11: (PlainEnum) -> ()

  var var_FunctionType12: (PlainProtocol) -> ()
// CHECK-LABEL: {{^}}  var var_FunctionType12: (PlainProtocol) -> ()

  var var_FunctionType13: (Class_ObjC1) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType13: (Class_ObjC1) -> ()

  var var_FunctionType14: (Protocol_Class1) -> ()
// CHECK-LABEL: {{^}}  var var_FunctionType14: (Protocol_Class1) -> ()

  var var_FunctionType15: (Protocol_ObjC1) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType15: (Protocol_ObjC1) -> ()

  var var_FunctionType16: (AnyObject) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType16: (AnyObject) -> ()

  var var_FunctionType17: (() -> ()) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType17: (() -> ()) -> ()

  var var_FunctionType18: ((Int) -> (), Int) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType18: ((Int) -> (), Int) -> ()

  var var_FunctionType19: ((String) -> (), Int) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionType19: ((String) -> (), Int) -> ()


  var var_FunctionTypeReturn1: () -> () -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionTypeReturn1: () -> () -> ()

  @objc var var_FunctionTypeReturn1_: () -> () -> () // no-error

  var var_FunctionTypeReturn2: () -> (Int) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionTypeReturn2: () -> (Int) -> ()

  @objc var var_FunctionTypeReturn2_: () -> (Int) -> () // no-error

  var var_FunctionTypeReturn3: () -> () -> Int
// CHECK-LABEL: {{^}}  @objc var var_FunctionTypeReturn3: () -> () -> Int

  @objc var var_FunctionTypeReturn3_: () -> () -> Int // no-error

  var var_FunctionTypeReturn4: () -> (String) -> ()
// CHECK-LABEL: {{^}}  @objc var var_FunctionTypeReturn4: () -> (String) -> ()

  @objc var var_FunctionTypeReturn4_: () -> (String) -> () // no-error

  var var_FunctionTypeReturn5: () -> () -> String
// CHECK-LABEL: {{^}}  @objc var var_FunctionTypeReturn5: () -> () -> String

  @objc var var_FunctionTypeReturn5_: () -> () -> String // no-error


  var var_BlockFunctionType1: @convention(block) () -> ()
// CHECK-LABEL: @objc var var_BlockFunctionType1: @convention(block) () -> ()

  @objc var var_BlockFunctionType1_: @convention(block) () -> () // no-error

  var var_ArrayType1: [AnyObject]
  // CHECK-LABEL: {{^}}  @objc var var_ArrayType1: [AnyObject]

  @objc var var_ArrayType1_: [AnyObject] // no-error

  var var_ArrayType2: [@convention(block) AnyObject -> AnyObject] // no-error
  // CHECK-LABEL: {{^}}  @objc var var_ArrayType2: [@convention(block) AnyObject -> AnyObject]

  @objc var var_ArrayType2_: [@convention(block) AnyObject -> AnyObject] // no-error

  var var_ArrayType3: [PlainStruct]
  // CHECK-LABEL: {{^}}  var var_ArrayType3: [PlainStruct]

  @objc var var_ArrayType3_: [PlainStruct]
  // expected-error @-1{{property cannot be marked @objc because its type cannot be represented in Objective-C}}

  var var_ArrayType4: [AnyObject -> AnyObject] // no-error
  // CHECK-LABEL: {{^}}  var var_ArrayType4: [AnyObject -> AnyObject]

  @objc var var_ArrayType4_: [AnyObject -> AnyObject] 
  // expected-error @-1{{property cannot be marked @objc because its type cannot be represented in Objective-C}}
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
  init() {}

  var var_GP_Unconstrained: GP_Unconstrained
// CHECK-LABEL: {{^}}  var var_GP_Unconstrained: GP_Unconstrained

  @objc var var_GP_Unconstrained_: GP_Unconstrained
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_PlainClass: GP_PlainClass
// CHECK-LABEL: {{^}}  var var_GP_PlainClass: GP_PlainClass

  @objc var var_GP_PlainClass_: GP_PlainClass
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_PlainProtocol: GP_PlainProtocol
// CHECK-LABEL: {{^}}  var var_GP_PlainProtocol: GP_PlainProtocol

  @objc var var_GP_PlainProtocol_: GP_PlainProtocol
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Class_ObjC: GP_Class_ObjC
// CHECK-LABEL: {{^}}  var var_GP_Class_ObjC: GP_Class_ObjC

  @objc var var_GP_Class_ObjC_: GP_Class_ObjC
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Protocol_Class: GP_Protocol_Class
// CHECK-LABEL: {{^}}  var var_GP_Protocol_Class: GP_Protocol_Class

  @objc var var_GP_Protocol_Class_: GP_Protocol_Class
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  var var_GP_Protocol_ObjC: GP_Protocol_ObjC
// CHECK-LABEL: {{^}}  var var_GP_Protocol_ObjC: GP_Protocol_ObjC

  @objc var var_GP_Protocol_ObjCa: GP_Protocol_ObjC
  // expected-error@-1 {{property cannot be marked @objc because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}

  func func_GP_Unconstrained(a: GP_Unconstrained) {}
// CHECK-LABEL: {{^}} func func_GP_Unconstrained(a: GP_Unconstrained) {

  @objc func func_GP_Unconstrained_(a: GP_Unconstrained) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{generic type parameters cannot be represented in Objective-C}}
  // expected-error@-3 {{method in a generic class cannot be represented in Objective-C}}
}

class infer_instanceVar3 : Class_ObjC1 {
// CHECK-LABEL: @objc class infer_instanceVar3 : Class_ObjC1 {

  var v1: Int = 0
// CHECK-LABEL: @objc var v1: Int
}


@objc
protocol infer_instanceVar4 {
// CHECK-LABEL: @objc protocol infer_instanceVar4 {

  var v1: Int { get }
// CHECK-LABEL: @objc var v1: Int { get }
}

// @!objc
class infer_instanceVar5 {
// CHECK-LABEL: {{^}}class infer_instanceVar5 {

  @objc
  var intstanceVar1: Int {
  // CHECK: @objc var intstanceVar1: Int
    get {}
    // CHECK: @objc get {}
    set {}
    // CHECK: @objc set {}
  }
}

@objc
class infer_staticVar1 {
// CHECK-LABEL: @objc class infer_staticVar1 {

  class var staticVar1: Int = 42 // expected-error {{class stored properties not yet supported}}
  // CHECK: @objc class var staticVar1: Int
}

// @!objc
class infer_subscript1 {
// CHECK-LABEL: class infer_subscript1

  @objc
  subscript(i: Int) -> Int {
  // CHECK: @objc subscript (i: Int) -> Int
    get {}
    // CHECK: @objc get {}
    set {}
    // CHECK: @objc set {}
  }
}


@objc
protocol infer_throughConformanceProto1 {
// CHECK-LABEL: @objc protocol infer_throughConformanceProto1 {

  func funcObjC1()
  var varObjC1: Int { get }
  var varObjC2: Int { get set }
  // CHECK: @objc func funcObjC1()
  // CHECK: @objc var varObjC1: Int { get }
  // CHECK: @objc var varObjC2: Int { get set }
}

class infer_class1 : PlainClass {}
// CHECK-LABEL: {{^}}class infer_class1 : PlainClass {

class infer_class2 : Class_ObjC1 {}
// CHECK-LABEL: @objc class infer_class2 : Class_ObjC1 {

class infer_class3 : infer_class2 {}
// CHECK-LABEL: @objc class infer_class3 : infer_class2 {

class infer_class4 : Protocol_Class1 {}
// CHECK-LABEL: {{^}}class infer_class4 : Protocol_Class1 {

class infer_class5 : Protocol_ObjC1 {}
// CHECK-LABEL: {{^}}class infer_class5 : Protocol_ObjC1 {

//
// If a protocol conforms to an @objc protocol, this does not infer @objc on
// the protocol itself, or on the newly introduced requirements.  Only the
// inherited @objc requirements get @objc.
//
// Same rule applies to classes.
//

protocol infer_protocol1 {
// CHECK-LABEL: {{^}}protocol infer_protocol1 {

  func func_Curried1()() // no-error
  // CHECK: {{^}} func func_Curried1()()

  func nonObjC1()
  // CHECK: {{^}} func nonObjC1()
}

protocol infer_protocol2 : Protocol_Class1 {
// CHECK-LABEL: {{^}}protocol infer_protocol2 : Protocol_Class1 {

  func func_Curried1()() // no-error
  // CHECK: {{^}} func func_Curried1()()

  func nonObjC1()
  // CHECK: {{^}} func nonObjC1()
}

protocol infer_protocol3 : Protocol_ObjC1 {
// CHECK-LABEL: {{^}}protocol infer_protocol3 : Protocol_ObjC1 {

  func func_Curried1()() // no-error
  // CHECK: {{^}} func func_Curried1()()

  func nonObjC1()
  // CHECK: {{^}} func nonObjC1()
}

protocol infer_protocol4 : Protocol_Class1, Protocol_ObjC1 {
// CHECK-LABEL: {{^}}protocol infer_protocol4 : Protocol_Class1, Protocol_ObjC1 {

  func func_Curried1()() // no-error
  // CHECK: {{^}} func func_Curried1()()

  func nonObjC1()
  // CHECK: {{^}} func nonObjC1()
}

protocol infer_protocol5 : Protocol_ObjC1, Protocol_Class1 {
// CHECK-LABEL: {{^}}protocol infer_protocol5 : Protocol_ObjC1, Protocol_Class1 {

  func func_Curried1()() // no-error
  // CHECK: {{^}} func func_Curried1()()

  func nonObjC1()
  // CHECK: {{^}} func nonObjC1()
}

class C {
  // Don't crash.
  @objc func foo(x: Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
  @IBAction func myAction(sender: Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
}

//===---
//===--- @IBOutlet implies @objc
//===---

class HasIBOutlet {
// CHECK-LABEL: {{^}}class HasIBOutlet {

  init() {}

  @IBOutlet weak var goodOutlet: Class_ObjC1!
  // CHECK-LABEL: {{^}} @IBOutlet @objc weak var goodOutlet: @sil_weak Class_ObjC1!

  @IBOutlet var badOutlet: PlainStruct
  // expected-error@-1 {{@IBOutlet property cannot have non-object type 'PlainStruct'}}
  // CHECK-LABEL: {{^}}  @IBOutlet var badOutlet: PlainStruct
}

//===---
//===--- @NSManaged implies @objc
//===---

class HasNSManaged {
// CHECK-LABEL: {{^}}class HasNSManaged {

  init() {}

  @NSManaged
  var goodManaged: Class_ObjC1
  // CHECK-LABEL: {{^}}  @NSManaged @objc dynamic var goodManaged: Class_ObjC1

  @NSManaged
  var badManaged: PlainStruct
  // expected-error@-1 {{property cannot be marked @NSManaged because its type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  // CHECK-LABEL: {{^}}  @NSManaged var badManaged: PlainStruct
}

//===---
//===--- Pointer argument types
//===---

@objc class TakesCPointers {
// CHECK-LABEL: {{^}}@objc class TakesCPointers {
  func constUnsafeMutablePointer(p: UnsafePointer<Int>) {}
  // CHECK-LABEL: @objc func constUnsafeMutablePointer(p: UnsafePointer<Int>) {

  func constUnsafeMutablePointerToAnyObject(p: UnsafePointer<AnyObject>) {}
  // CHECK-LABEL: @objc func constUnsafeMutablePointerToAnyObject(p: UnsafePointer<AnyObject>) {

  func constUnsafeMutablePointerToClass(p: UnsafePointer<TakesCPointers>) {}
  // CHECK-LABEL: @objc func constUnsafeMutablePointerToClass(p: UnsafePointer<TakesCPointers>) {

  func mutableUnsafeMutablePointer(p: UnsafeMutablePointer<Int>) {}
  // CHECK-LABEL: @objc func mutableUnsafeMutablePointer(p: UnsafeMutablePointer<Int>) {

  func mutableStrongUnsafeMutablePointerToAnyObject(p: UnsafeMutablePointer<AnyObject>) {}
  // CHECK-LABEL: {{^}} @objc func mutableStrongUnsafeMutablePointerToAnyObject(p: UnsafeMutablePointer<AnyObject>) {

  func mutableAutoreleasingUnsafeMutablePointerToAnyObject(p: AutoreleasingUnsafeMutablePointer<AnyObject>) {}
  // CHECK-LABEL: {{^}} @objc func mutableAutoreleasingUnsafeMutablePointerToAnyObject(p: AutoreleasingUnsafeMutablePointer<AnyObject>) {

  func cFunctionPointer(p: CFunctionPointer<() -> ()>) {} // expected-error{{unavailable}}
  // CHECK-LABEL: {{^}} func cFunctionPointer(p: <<error type>>) -> <<error type>>
}

// @objc with nullary names
@objc(NSObjC2)
class Class_ObjC2 {
// CHECK-LABEL: @objc(NSObjC2) class Class_ObjC2

  @objc(initWithMalice)
  init(foo: ()) { }

  @objc(initWithIntent)
  init(bar _: ()) { }

  @objc(initForMurder)
  init() { }

  @objc(isFoo)
  func foo() -> Bool {}
  // CHECK-LABEL: @objc(isFoo) func foo() -> Bool {
}

@objc() // expected-error{{expected name within parentheses of @objc attribute}}
class Class_ObjC3 { 
}

// @objc with selector names
extension PlainClass {
  // CHECK-LABEL: @objc(setFoo:) dynamic func
  @objc(setFoo:)
  func foo(b: Bool) { }

  // CHECK-LABEL: @objc(setWithRed:green:blue:alpha:) dynamic func set
  @objc(setWithRed:green:blue:alpha:)
  func set(Float, green: Float, blue: Float, alpha: Float)  { }

  // CHECK-LABEL: @objc(createWithRed:green:blue:alpha:) dynamic class func createWith
  @objc(createWithRed:green blue:alpha)
  class func createWithRed(Float, green: Float, blue: Float, alpha: Float) { }
  // expected-error@-2{{missing ':' after selector piece in @objc attribute}}{{28-28=:}}
  // expected-error@-3{{missing ':' after selector piece in @objc attribute}}{{39-39=:}}

  // CHECK-LABEL: @objc(::) dynamic func badlyNamed
  @objc(::)
  func badlyNamed(Int, y: Int) {}
}

@objc(Class:) // expected-error{{'@objc' class must have a simple name}}{{12-13=}}
class BadClass1 { }

@objc(Protocol:) // expected-error{{'@objc' protocol must have a simple name}}{{15-16=}}
protocol BadProto1 { }

class BadClass2 {
  @objc(badprop:foo:wibble:) // expected-error{{'@objc' property must have a simple name}}{{16-28=}}
  var badprop: Int = 5

  @objc(foo) // expected-error{{'@objc' subscript cannot have a name; did you mean to put the name on the getter or setter?}}
  subscript (i: Int) -> Int {
    get {
      return i
    }
  }

  @objc(foo) // expected-error{{'@objc' method name provides names for 0 arguments, but method has one parameter}}
  func noArgNamesOneParam(x: Int) { }
  
  @objc(foo) // expected-error{{'@objc' method name provides names for 0 arguments, but method has one parameter}}
  func noArgNamesOneParam2(Int) { }

  @objc(foo) // expected-error{{'@objc' method name provides names for 0 arguments, but method has 2 parameters}}
  func noArgNamesTwoParams(Int, y: Int) { }

  @objc(foo:) // expected-error{{'@objc' method name provides one argument name, but method has 2 parameters}}
  func oneArgNameTwoParams(Int, y: Int) { }

  @objc(foo:) // expected-error{{'@objc' method name provides one argument name, but method has 0 parameters}}
  func oneArgNameNoParams() { }

  @objc(foo:) // expected-error{{'@objc' initializer name provides one argument name, but initializer has 0 parameters}}
  init() { }

  var _prop = 5
  @objc var prop: Int {
    @objc(property) get { return _prop }
    @objc(setProperty:) set { _prop = newValue }
  }

  var prop2: Int {
    @objc(property) get { return _prop } // expected-error{{'@objc' getter for non-'@objc' property}}
    @objc(setProperty:) set { _prop = newValue } // expected-error{{'@objc' setter for non-'@objc' property}}
  }

  var prop3: Int {
    @objc(setProperty:) didSet { } // expected-error{{observing accessors are not allowed to be marked @objc}}
  }

  @objc
  subscript (c: Class_ObjC1) -> Class_ObjC1 {
    @objc(getAtClass:) get {
      return c
    }

    @objc(setAtClass:class:) set {
    }
  }
}

// Swift overrides that aren't also @objc overrides.
class Super {
  @objc(renamedFoo)
  var foo: Int { get { return 3 } } // expected-note 2{{overridden declaration is here}}
}

class Sub1 : Super {
  @objc(foo) // expected-error{{Objective-C property has a different name from the property it overrides ('foo' vs. 'renamedFoo')}}{{9-12=renamedFoo}}
  override var foo: Int { get { return 5 } }
}

class Sub2 : Super {
  @objc
  override var foo: Int { get { return 5 } }
}

class Sub3 : Super {
  override var foo: Int { get { return 5 } }
}

class Sub4 : Super {
  @objc(renamedFoo)
  override var foo: Int { get { return 5 } }
}

class Sub5 : Super {
  @objc(wrongFoo) // expected-error{{Objective-C property has a different name from the property it overrides ('wrongFoo' vs. 'renamedFoo')}}
  override var foo: Int { get { return 5 } }
}

enum NotObjCEnum { case X }
struct NotObjCStruct {}

// Closure arguments can only be @objc if their parameters and returns are.
// CHECK-LABEL: @objc class ClosureArguments
@objc class ClosureArguments {
  // CHECK: @objc func foo
  @objc func foo(f: Int -> ()) {}
  // CHECK: @objc func bar
  @objc func bar(f: NotObjCEnum -> NotObjCStruct) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{function types cannot be represented in Objective-C unless their parameters and returns can be}}
  // CHECK: @objc func bas
  @objc func bas(f: NotObjCEnum -> ()) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{function types cannot be represented in Objective-C unless their parameters and returns can be}}
  // CHECK: @objc func zim
  @objc func zim(f: () -> NotObjCStruct) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{function types cannot be represented in Objective-C unless their parameters and returns can be}}
  // CHECK: @objc func zang
  @objc func zang(f: (NotObjCEnum, NotObjCStruct) -> ()) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{function types cannot be represented in Objective-C unless their parameters and returns can be}}
  // CHECK: @objc func fooImplicit
  func fooImplicit(f: Int -> ()) {}
  // CHECK: {{^}}  func barImplicit
  func barImplicit(f: NotObjCEnum -> NotObjCStruct) {}
  // CHECK: {{^}}  func basImplicit
  func basImplicit(f: NotObjCEnum -> ()) {}
  // CHECK: {{^}}  func zimImplicit
  func zimImplicit(f: () -> NotObjCStruct) {}
  // CHECK: {{^}}  func zangImplicit
  func zangImplicit(f: (NotObjCEnum, NotObjCStruct) -> ()) {}
}

typealias GoodBlock = @convention(block) Int -> ()
typealias BadBlock = @convention(block) NotObjCEnum -> () // expected-error{{@convention(block) type is not representable in Objective-C}}


@objc class AccessControl {
  // CHECK: @objc func foo
  func foo() {}
  // CHECK: {{^}} private func bar
  private func bar() {}
  // CHECK: @objc private func baz
  @objc private func baz() {}
}

//===--- Ban @objc +load methods
class Load1 {
  // Okay: not @objc
  class func load() { }
}

@objc class Load2 {
  class func load() { } // expected-error{{method 'load()' defines Objective-C class method 'load', which is not permitted by Swift}}
}

@objc class Load3 {
  class var load: Load3 {
    get { return Load3() } // expected-error{{getter for 'load' defines Objective-C class method 'load', which is not permitted by Swift}}
    set { }
  }
}

// Members of protocol extensions cannot be @objc

extension PlainProtocol {
  @objc final var property: Int { return 5 } // expected-error{{variable in a protocol extension cannot be represented in Objective-C}}
  @objc final subscript(x: Int) -> Class_ObjC1 { return Class_ObjC1() } // expected-error{{subscript in a protocol extension cannot be represented in Objective-C}}
  @objc final func fun() { } // expected-error{{method in a protocol extension cannot be represented in Objective-C}}
}

extension Protocol_ObjC1 {
  // Don't infer @objc for extensions of @objc protocols.

  // CHECK: {{^}} var property: Int
  final var property: Int { return 5 }
}

//===---
//===--- Error handling
//===---
class ClassThrows1 {
  // CHECK: @objc func methodReturnsVoid() throws
  @objc func methodReturnsVoid() throws { }

  // CHECK: @objc func methodReturnsObjCClass() throws -> Class_ObjC1
  @objc func methodReturnsObjCClass() throws -> Class_ObjC1 {
    return Class_ObjC1()
  }

  // CHECK: @objc func methodReturnsBridged() throws -> String
  @objc func methodReturnsBridged() throws -> String { return String() }

  // CHECK: @objc func methodReturnsArray() throws -> [String]
  @objc func methodReturnsArray() throws -> [String] { return [String]() }

  // CHECK: @objc init(degrees: Double) throws
  @objc init(degrees: Double) throws { }

  // Errors

  @objc func methodReturnsOptionalObjCClass() throws -> Class_ObjC1? { return nil } // expected-error{{throwing method cannot be marked @objc because it returns a value of optional type 'Class_ObjC1?'; 'nil' indicates failure to Objective-C}}

  @objc func methodReturnsOptionalArray() throws -> [String]? { return nil } // expected-error{{throwing method cannot be marked @objc because it returns a value of optional type '[String]?'; 'nil' indicates failure to Objective-C}}

  @objc func methodReturnsInt() throws -> Int { return 0 } // expected-error{{throwing method cannot be marked @objc because it returns a value of type 'Int'; return 'Void' or a type that bridges to an Objective-C class}}

  @objc func methodAcceptsThrowingFunc(fn: (String) throws -> Int) { }
  // expected-error@-1{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2{{throwing function types cannot be represented in Objective-C}}

  @objc init?(radians: Double) throws { } // expected-error{{a failable and throwing initializer cannot be marked @objc because 'nil' indicates failure to Objective-C}}

  @objc init!(string: String) throws { } // expected-error{{a failable and throwing initializer cannot be marked @objc because 'nil' indicates failure to Objective-C}}
}


// CHECK-DUMP-LABEL: class_decl "ImplicitClassThrows1"
@objc class ImplicitClassThrows1 {
  // CHECK: @objc func methodReturnsVoid() throws
  // CHECK-DUMP: func_decl "methodReturnsVoid()"{{.*}}foreign_error=ZeroResult,unowned,param=0,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  func methodReturnsVoid() throws { }

  // CHECK: @objc func methodReturnsObjCClass() throws -> Class_ObjC1
  // CHECK-DUMP: func_decl "methodReturnsObjCClass()" {{.*}}foreign_error=NilResult,unowned,param=0,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>
  func methodReturnsObjCClass() throws -> Class_ObjC1 {
    return Class_ObjC1()
  }

  // CHECK: @objc func methodReturnsBridged() throws -> String
  func methodReturnsBridged() throws -> String { return String() }

  // CHECK: @objc func methodReturnsArray() throws -> [String]
  func methodReturnsArray() throws -> [String] { return [String]() }

  // CHECK: {{^}} func methodReturnsOptionalObjCClass() throws -> Class_ObjC1?
  func methodReturnsOptionalObjCClass() throws -> Class_ObjC1? { return nil }

  // CHECK: @objc func methodWithTrailingClosures(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int, fn3: (Int) -> Int)
  // CHECK-DUMP: func_decl "methodWithTrailingClosures(_:fn1:fn2:fn3:)"{{.*}}foreign_error=ZeroResult,unowned,param=1,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  func methodWithTrailingClosures(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int, fn3: (Int) -> Int) throws { }

  // CHECK: @objc init(degrees: Double) throws
  // CHECK-DUMP: constructor_decl "init(degrees:)"{{.*}}foreign_error=NilResult,unowned,param=1,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>
  init(degrees: Double) throws { }
}

// CHECK-DUMP-LABEL: class_decl "SubclassImplicitClassThrows1"
@objc class SubclassImplicitClassThrows1 : ImplicitClassThrows1 {
  // CHECK: @objc override func methodWithTrailingClosures(s: String, fn1: ((Int) -> Int), fn2: ((Int) -> Int), fn3: ((Int) -> Int))
  // CHECK-DUMP: func_decl "methodWithTrailingClosures(_:fn1:fn2:fn3:)"{{.*}}foreign_error=ZeroResult,unowned,param=1,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  override func methodWithTrailingClosures(s: String, fn1: ((Int) -> Int), fn2: ((Int) -> Int), fn3: ((Int) -> Int)) throws { }
}

class ThrowsRedecl1 {
  @objc func method1(x: Int, error: Class_ObjC1) { } // expected-note{{declared here}}
  @objc func method1(x: Int) throws { } // expected-error{{with Objective-C selector 'method1:error:'}}

  @objc func method2AndReturnError(x: Int) { } // expected-note{{declared here}}
  @objc func method2() throws { } // expected-error{{with Objective-C selector 'method2AndReturnError:'}}

  @objc func method3(x: Int, error: Int, closure: Int -> Int) { }  // expected-note{{declared here}}
  @objc func method3(x: Int, closure: Int -> Int) throws { } // expected-error{{with Objective-C selector 'method3:error:closure:'}}

  @objc(initAndReturnError:) func initMethod1(error: Int) { } // expected-note{{declared here}}
  @objc init() throws { } // expected-error{{with Objective-C selector 'initAndReturnError:'}}

  @objc(initWithString:error:) func initMethod2(string: String, error: Int) { } // expected-note{{declared here}}
  @objc init(string: String) throws { } // expected-error{{with Objective-C selector 'initWithString:error:'}}

  @objc(initAndReturnError:fn:) func initMethod3(error: Int, fn: Int -> Int) { } // expected-note{{declared here}}
  @objc init(fn: Int -> Int) throws { } // expected-error{{with Objective-C selector 'initAndReturnError:fn:'}}
}

class ThrowsObjCName {
  @objc(method4:closure:error:) func method4(x: Int, closure: Int -> Int) throws { }

  @objc(method5AndReturnError:x:closure:) func method5(x: Int, closure: Int -> Int) throws { }

  @objc(method6) func method6() throws { } // expected-error{{@objc' method name provides names for 0 arguments, but method has one parameter (the error parameter)}}

  @objc(method7) func method7(x: Int) throws { } // expected-error{{@objc' method name provides names for 0 arguments, but method has 2 parameters (including the error parameter)}}

  // CHECK-DUMP: func_decl "method8(_:fn1:fn2:)"{{.*}}foreign_error=ZeroResult,unowned,param=2,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  @objc(method8:fn1:error:fn2:)
  func method8(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int) throws { }

  // CHECK-DUMP: func_decl "method9(_:fn1:fn2:)"{{.*}}foreign_error=ZeroResult,unowned,param=0,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  @objc(method9AndReturnError:s:fn1:fn2:)
  func method9(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int) throws { }
}

class SubclassThrowsObjCName : ThrowsObjCName {
  // CHECK-DUMP: func_decl "method8(_:fn1:fn2:)"{{.*}}foreign_error=ZeroResult,unowned,param=2,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  override func method8(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int) throws { }

  // CHECK-DUMP: func_decl "method9(_:fn1:fn2:)"{{.*}}foreign_error=ZeroResult,unowned,param=0,paramtype=AutoreleasingUnsafeMutablePointer<Optional<NSError>>,resulttype=Bool
  override func method9(s: String, fn1: ((Int) -> Int), fn2: (Int) -> Int) throws { }
}
