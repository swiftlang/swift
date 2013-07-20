// RUN: %swift -verify -parse %s

class ThisBase1 {
  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  // FIXME: uncomment when we have static vars.
  // static var baseStaticVar : Int

  static func baseStaticFunc0() {}

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  oneof BaseNestedOneof {
    case BaseOneofX(Int)
  }

  typealias BaseNestedTypealias = Int
}

class ThisDerived1 : ThisBase1 {
  var derivedInstanceVar: Int

  var derivedProp : Int {
  get:
    return 42
  set(val):
  }

  func derivedFunc0() {}
  func derivedFunc1(a: Int) {}

  subscript(i: Double) -> Int { // expected-error {{cannot overload a declaration from a base class}}
  get:
    return Int(i)
  set(val):
    baseInstanceVar = Int(i)
  }

  static func derivedStaticFunc0() {}

  struct DerivedNestedStruct {}
  class DerivedNestedClass {}
  oneof DerivedNestedOneof {
    case DerivedOneofX(Int)
  }

  typealias DerivedNestedTypealias = Int

  func test1() {
    this.baseInstanceVar = 42
    this.baseProp = 42
    this.baseFunc0()
    this.baseFunc1(42)
    this[0] = 42.0
    this.baseStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseStaticFunc0'}}

    this.baseExtProp = 42
    this.baseExtFunc0()
    this.baseExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseExtStaticFunc0'}}

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedOneof = .BaseOneofX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = this.BaseNestedStruct()
    var bc2 = this.BaseNestedClass()
    var bo2 = this.BaseOneofX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseOneofX'}}
    var bo3 = this.BaseNestedOneof.BaseOneofX(24)
    var bt2 = this.BaseNestedTypealias(42)

    var bes1 : BaseExtNestedStruct
    var bec1 : BaseExtNestedClass
    var beo1 : BaseExtNestedOneof = .BaseExtOneofX(42)
    var bet1 : BaseExtNestedTypealias
    var bes2 = this.BaseExtNestedStruct()
    var bec2 = this.BaseExtNestedClass()
    var beo2 = this.BaseExtOneofX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseExtOneofX'}}
    var beo3 = this.BaseExtNestedOneof.BaseExtOneofX(24)
    var bet2 = this.BaseExtNestedTypealias(42)

    this.derivedInstanceVar = 42
    this.derivedProp = 42
    this.derivedFunc0()
    this.derivedStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedStaticFunc0'}}

    this.derivedExtProp = 42
    this.derivedExtFunc0()
    this.derivedExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedExtStaticFunc0'}}

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedOneof = .DerivedOneofX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = this.DerivedNestedStruct()
    var dc2 = this.DerivedNestedClass()
    var do2 = this.DerivedOneofX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedOneofX'}}
    var do3 = this.DerivedNestedOneof.DerivedOneofX(24)
    var dt2 = this.DerivedNestedTypealias(42)

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedOneof = .DerivedExtOneofX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = this.DerivedExtNestedStruct()
    var dec2 = this.DerivedExtNestedClass()
    var deo2 = this.DerivedExtOneofX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedExtOneofX'}}
    var deo3 = this.DerivedExtNestedOneof.DerivedExtOneofX(24)
    var det2 = this.DerivedExtNestedTypealias(42)

    this.metatype // expected-error {{expected field name following '.'}}
  }

  static func staticTest1() {
    this.baseInstanceVar = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'baseInstanceVar'}}
    this.baseProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'baseProp'}}
    this.baseFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    this.baseFunc0(ThisBase1())()
    this.baseFunc1(42) // expected-error {{expression does not type-check}}
    this.baseFunc1(ThisBase1())(42)
    this[0] = 42.0 // expected-error {{'ThisDerived1.metatype' does not have a member named '__subscript'}}
    this.baseStaticFunc0()

    this.baseExtProp = 42 // expected-error {{ThisDerived1.metatype' does not have a member named 'baseExtProp'}}
    this.baseExtFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    this.baseExtStaticFunc0()

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedOneof = .BaseOneofX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = this.BaseNestedStruct()
    var bc2 = this.BaseNestedClass()
    var bo2 = this.BaseOneofX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'BaseOneofX'}}
    var bo3 = this.BaseNestedOneof.BaseOneofX(24)
    var bt2 = this.BaseNestedTypealias(42)

    this.derivedInstanceVar = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedInstanceVar'}}
    this.derivedProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedProp'}}
    this.derivedFunc0() // expected-error {{'()' is not convertible to 'ThisDerived1'}}
    this.derivedFunc0(ThisBase1())() // expected-error {{'ThisBase1' is not convertible to 'ThisDerived1'}}
    this.derivedFunc0(ThisDerived1())()
    this.derivedStaticFunc0()

    this.derivedExtProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedExtProp'}}
    this.derivedExtFunc0() // expected-error {{'()' is not convertible to 'ThisDerived1'}}
    this.derivedExtStaticFunc0()

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedOneof = .DerivedOneofX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = this.DerivedNestedStruct()
    var dc2 = this.DerivedNestedClass()
    var do2 = this.DerivedOneofX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'DerivedOneofX'}}
    var do3 = this.DerivedNestedOneof.DerivedOneofX(24)
    var dt2 = this.DerivedNestedTypealias(42)

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedOneof = .DerivedExtOneofX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = this.DerivedExtNestedStruct()
    var dec2 = this.DerivedExtNestedClass()
    var deo2 = this.DerivedExtOneofX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'DerivedExtOneofX'}}
    var deo3 = this.DerivedExtNestedOneof.DerivedExtOneofX(24)
    var det2 = this.DerivedExtNestedTypealias(42)

    this.metatype // expected-error {{expected field name following '.'}}
  }
}

extension ThisBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  oneof BaseExtNestedOneof {
    case BaseExtOneofX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

extension ThisDerived1 {
  var derivedExtProp : Int {
  get:
    return 42
  set(val):
  }

  func derivedExtFunc0() {}

  static func derivedExtStaticFunc0() {}

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {}
  oneof DerivedExtNestedOneof {
    case DerivedExtOneofX(Int)
  }

  typealias DerivedExtNestedTypealias = Int
}

