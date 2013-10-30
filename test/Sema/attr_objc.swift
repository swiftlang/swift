// RUN: %swift %s -verify

class PlainClass {}
struct PlainStruct {}
enum PlainEnum {}
protocol PlainProtocol {}

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
var subject_globalVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

var subject_getterSetter: Int {
@objc get: return 0 // expected-error {{only classes and their methods can be declared 'objc'}}
@objc set: // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc
func subject_freeFunc() { // expected-error {{only classes and their methods can be declared 'objc'}}
  @objc
  var subject_localVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_nestedFreeFunc() { // expected-error {{only classes and their methods can be declared 'objc'}}
  }
}

@objc
func subject_genericFunc<T>(t: T) { // expected-error {{only classes and their methods can be declared 'objc'}}
  @objc
  var subject_localVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc
struct subject_struct { // expected-error {{only classes and their methods can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc
struct subject_genericStruct<T> { // expected-error {{only classes and their methods can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
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
class subject_genericClass<T> { // expected-error {{only classes and their methods can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc
enum subject_enum { // expected-error {{only classes and their methods can be declared 'objc'}}
  // FIXME
  @objc
  case subject_enumElement // e/xpected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc
enum subject_genericEnum<T> { // expected-error {{only classes and their methods can be declared 'objc'}}
  // FIXME
  @objc
  case subject_enumElement // e/xpected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  init() {} // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() {} // expected-error {{only classes and their methods can be declared 'objc'}}
}


@objc
protocol subject_protocol1 { // expected-error {{only [class_protocol] protocols can be declared 'objc'}}
  @objc
  var subject_instanceVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() // expected-error {{only classes and their methods can be declared 'objc'}}
}

@objc @class_protocol
protocol subject_protocol2 { // no-error
}

@class_protocol @objc
protocol subject_protocol3 {} // no-error

@objc
protocol subject_protocol4 : PlainProtocol {} // expected-error {{only [class_protocol] protocols can be declared 'objc'}}

@objc
protocol subject_protocol5 : Protocol_Class1 {} // expected-error {{[objc] protocol 'subject_protocol5' cannot refine non-[objc] protocol 'Protocol_Class1'}}

@objc
protocol subject_protocol6 : Protocol_ObjC1 {}

protocol subject_containerProtocol1 {
  @objc
  var subject_instanceVar: Int // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  func subject_instanceFunc() // expected-error {{only classes and their methods can be declared 'objc'}}

  @objc
  static func subject_staticFunc() // expected-error {{only classes and their methods can be declared 'objc'}}
}

func genericContext1<T>() {
  @objc
  class subject_inGenericContext {} // expected-error {{only classes and their methods can be declared 'objc'}}

  class subject_constructor_inGenericContext {
    @objc
    init() {} // expected-error {{only classes and their methods can be declared 'objc'}}
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
  class subject_inGenericContext {} // expected-error {{only classes and their methods can be declared 'objc'}}
}

class GenericContext3<T> {
  class MoreNested {
    @objc
    class subject_inGenericContext {} // expected-error {{only classes and their methods can be declared 'objc'}}
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
  subscript(a: Float32) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid2 {
  @objc
  subscript(a: PlainStruct) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid3 {
  @objc
  subscript(a: PlainEnum) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid4 {
  @objc
  subscript(a: PlainProtocol) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid5 {
  @objc
  subscript(a: Protocol_Class1) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid6 {
  @objc
  subscript(a: protocol<Protocol_Class1, Protocol_Class2>) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}
class subject_subscriptInvalid7 {
  @objc
  subscript(a: protocol<Protocol_ObjC1, Protocol_ObjC2>) -> Int { // expected-error {{only classes and their methods can be declared 'objc'}}
  get: return 0
  }
}

