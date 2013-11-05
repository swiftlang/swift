// RUN: %swift -verify -parse %s

class ThisBase1 {
  init() { }

  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  def baseFunc0() {}
  def baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  // FIXME: uncomment when we have static vars.
  // static var baseStaticVar : Int

  static def baseStaticFunc0() {}

  struct BaseNestedStruct {}
  class BaseNestedClass {
    init() { }
  }
  enum BaseNestedUnion {
    case BaseUnionX(Int)
  }

  typealias BaseNestedTypealias = Int
}

class ThisDerived1 : ThisBase1 {
  init() { super.init() }

  var derivedInstanceVar: Int

  var derivedProp : Int {
  get:
    return 42
  set(val):
  }

  def derivedFunc0() {}
  def derivedFunc1(a: Int) {}

  subscript(i: Double) -> Int { // expected-error {{cannot overload a declaration from a superclass}}
  get:
    return Int(i)
  set(val):
    baseInstanceVar = Int(i)
  }

  static def derivedStaticFunc0() {}

  struct DerivedNestedStruct {}
  class DerivedNestedClass {
    init() { }
  }
  enum DerivedNestedUnion {
    case DerivedUnionX(Int)
  }

  typealias DerivedNestedTypealias = Int

  def testThis1() {
    self.baseInstanceVar = 42
    self.baseProp = 42
    self.baseFunc0()
    self.baseFunc1(42)
    self[0] = 42.0
    self.baseStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseStaticFunc0'}}

    self.baseExtProp = 42
    self.baseExtFunc0()
    self.baseExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseExtStaticFunc0'}}

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedUnion = .BaseUnionX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = self.BaseNestedStruct()
    var bc2 = self.BaseNestedClass()
    var bo2 = self.BaseUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseUnionX'}}
    var bo3 = self.BaseNestedUnion.BaseUnionX(24)
    var bt2 = self.BaseNestedTypealias(42)

    var bes1 : BaseExtNestedStruct
    var bec1 : BaseExtNestedClass
    var beo1 : BaseExtNestedUnion = .BaseExtUnionX(42)
    var bet1 : BaseExtNestedTypealias
    var bes2 = self.BaseExtNestedStruct()
    var bec2 = self.BaseExtNestedClass()
    var beo2 = self.BaseExtUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseExtUnionX'}}
    var beo3 = self.BaseExtNestedUnion.BaseExtUnionX(24)
    var bet2 = self.BaseExtNestedTypealias(42)

    self.derivedInstanceVar = 42
    self.derivedProp = 42
    self.derivedFunc0()
    self.derivedStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedStaticFunc0'}}

    self.derivedExtProp = 42
    self.derivedExtFunc0()
    self.derivedExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedExtStaticFunc0'}}

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedUnion = .DerivedUnionX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = self.DerivedNestedStruct()
    var dc2 = self.DerivedNestedClass()
    var do2 = self.DerivedUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedUnionX'}}
    var do3 = self.DerivedNestedUnion.DerivedUnionX(24)
    var dt2 = self.DerivedNestedTypealias(42)

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedUnion = .DerivedExtUnionX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = self.DerivedExtNestedStruct()
    var dec2 = self.DerivedExtNestedClass()
    var deo2 = self.DerivedExtUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = self.DerivedExtNestedUnion.DerivedExtUnionX(24)
    var det2 = self.DerivedExtNestedTypealias(42)

    self.metatype // expected-error {{expected member name following '.'}}
  }

  def testSuper1() {
    super.baseInstanceVar = 42
    super.baseProp = 42
    super.baseFunc0()
    super.baseFunc1(42)
    super[0] = 42.0
    super.baseStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'baseStaticFunc0'}}

    super.baseExtProp = 42
    super.baseExtFunc0()
    super.baseExtStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'baseExtStaticFunc0'}}

    var bs2 = super.BaseNestedStruct()
    var bc2 = super.BaseNestedClass()
    var bo2 = super.BaseUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'BaseUnionX'}}
    var bo3 = super.BaseNestedUnion.BaseUnionX(24)
    var bt2 = super.BaseNestedTypealias(42)

    var bes2 = super.BaseExtNestedStruct()
    var bec2 = super.BaseExtNestedClass()
    var beo2 = super.BaseExtUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'BaseExtUnionX'}}
    var beo3 = super.BaseExtNestedUnion.BaseExtUnionX(24)
    var bet2 = super.BaseExtNestedTypealias(42)

    super.derivedInstanceVar = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedInstanceVar'}}
    super.derivedProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedProp'}}
    super.derivedFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedFunc0'}}
    super.derivedStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedStaticFunc0'}}

    super.derivedExtProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedExtProp'}}
    super.derivedExtFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedExtFunc0'}}
    super.derivedExtStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedExtStaticFunc0'}}

    var ds2 = super.DerivedNestedStruct() // expected-error {{'ThisBase1' does not have a member named 'DerivedNestedStruct'}}
    var dc2 = super.DerivedNestedClass() // expected-error {{'ThisBase1' does not have a member named 'DerivedNestedClass'}}
    var do2 = super.DerivedUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'DerivedUnionX'}}
    var do3 = super.DerivedNestedUnion.DerivedUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'DerivedNestedUnion'}}
    var dt2 = super.DerivedNestedTypealias(42) // expected-error {{'ThisBase1' does not have a member named 'DerivedNestedTypealias'}}

    var des2 = super.DerivedExtNestedStruct() // expected-error {{'ThisBase1' does not have a member named 'DerivedExtNestedStruct'}}
    var dec2 = super.DerivedExtNestedClass() // expected-error {{'ThisBase1' does not have a member named 'DerivedExtNestedClass'}}
    var deo2 = super.DerivedExtUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = super.DerivedExtNestedUnion.DerivedExtUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'DerivedExtNestedUnion'}}
    var det2 = super.DerivedExtNestedTypealias(42) // expected-error {{'ThisBase1' does not have a member named 'DerivedExtNestedTypealias'}}

    super.metatype // expected-error {{expected identifier or 'init' after super '.' expression}}
  }

  static def staticTestThis1() {
    self.baseInstanceVar = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'baseInstanceVar'}}
    self.baseProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'baseProp'}}
    self.baseFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    self.baseFunc0(ThisBase1())()
    self.baseFunc1(42) // expected-error {{expression does not type-check}}
    self.baseFunc1(ThisBase1())(42)
    self[0] = 42.0 // expected-error {{'ThisDerived1.metatype' does not have a member named 'subscript'}}
    self.baseStaticFunc0()

    self.baseExtProp = 42 // expected-error {{ThisDerived1.metatype' does not have a member named 'baseExtProp'}}
    self.baseExtFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    self.baseExtStaticFunc0()

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedUnion = .BaseUnionX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = self.BaseNestedStruct()
    var bc2 = self.BaseNestedClass()
    var bo2 = self.BaseUnionX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'BaseUnionX'}}
    var bo3 = self.BaseNestedUnion.BaseUnionX(24)
    var bt2 = self.BaseNestedTypealias(42)

    self.derivedInstanceVar = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedInstanceVar'}}
    self.derivedProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedProp'}}
    self.derivedFunc0() // expected-error {{'()' is not convertible to 'ThisDerived1'}}
    self.derivedFunc0(ThisBase1())() // expected-error {{'ThisBase1' is not convertible to 'ThisDerived1'}}
    self.derivedFunc0(ThisDerived1())()
    self.derivedStaticFunc0()

    self.derivedExtProp = 42 // expected-error {{'ThisDerived1.metatype' does not have a member named 'derivedExtProp'}}
    self.derivedExtFunc0() // expected-error {{'()' is not convertible to 'ThisDerived1'}}
    self.derivedExtStaticFunc0()

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedUnion = .DerivedUnionX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = self.DerivedNestedStruct()
    var dc2 = self.DerivedNestedClass()
    var do2 = self.DerivedUnionX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'DerivedUnionX'}}
    var do3 = self.DerivedNestedUnion.DerivedUnionX(24)
    var dt2 = self.DerivedNestedTypealias(42)

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedUnion = .DerivedExtUnionX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = self.DerivedExtNestedStruct()
    var dec2 = self.DerivedExtNestedClass()
    var deo2 = self.DerivedExtUnionX(24) // expected-error {{'ThisDerived1.metatype' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = self.DerivedExtNestedUnion.DerivedExtUnionX(24)
    var det2 = self.DerivedExtNestedTypealias(42)

    self.metatype // expected-error {{expected member name following '.'}}
  }

  static def staticTestSuper1() {
    super.baseInstanceVar = 42 // expected-error {{'ThisBase1.metatype' does not have a member named 'baseInstanceVar'}}
    super.baseProp = 42 // expected-error {{'ThisBase1.metatype' does not have a member named 'baseProp'}}
    super.baseFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    super.baseFunc0(ThisBase1())()
    super.baseFunc1(42) // expected-error {{expression does not type-check}}
    super.baseFunc1(ThisBase1())(42)
    super[0] = 42.0 // expected-error {{'ThisBase1.metatype' does not have a member named 'subscript'}}
    super.baseStaticFunc0()

    super.baseExtProp = 42 // expected-error {{ThisBase1.metatype' does not have a member named 'baseExtProp'}}
    super.baseExtFunc0() // expected-error {{'()' is not convertible to 'ThisBase1'}}
    super.baseExtStaticFunc0()

    var bs2 = super.BaseNestedStruct()
    var bc2 = super.BaseNestedClass()
    var bo2 = super.BaseUnionX(24) // expected-error {{'ThisBase1.metatype' does not have a member named 'BaseUnionX'}}
    var bo3 = super.BaseNestedUnion.BaseUnionX(24)
    var bt2 = super.BaseNestedTypealias(42)

    super.derivedInstanceVar = 42 // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedInstanceVar'}}
    super.derivedProp = 42 // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedProp'}}
    super.derivedFunc0() // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedFunc0'}}
    super.derivedStaticFunc0() // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedStaticFunc0'}}

    super.derivedExtProp = 42 // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedExtProp'}}
    super.derivedExtFunc0() // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedExtFunc0'}}
    super.derivedExtStaticFunc0() // expected-error {{'ThisBase1.metatype' does not have a member named 'derivedExtStaticFunc0'}}

    var ds2 = super.DerivedNestedStruct() // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedNestedStruct'}}
    var dc2 = super.DerivedNestedClass() // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedNestedClass'}}
    var do2 = super.DerivedUnionX(24) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedUnionX'}}
    var do3 = super.DerivedNestedUnion.DerivedUnionX(24) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedNestedUnion'}}
    var dt2 = super.DerivedNestedTypealias(42) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedNestedTypealias'}}

    var des2 = super.DerivedExtNestedStruct() // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedExtNestedStruct'}}
    var dec2 = super.DerivedExtNestedClass() // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedExtNestedClass'}}
    var deo2 = super.DerivedExtUnionX(24) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = super.DerivedExtNestedUnion.DerivedExtUnionX(24) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedExtNestedUnion'}}
    var det2 = super.DerivedExtNestedTypealias(42) // expected-error {{'ThisBase1.metatype' does not have a member named 'DerivedExtNestedTypealias'}}

    super.metatype // expected-error {{expected identifier or 'init' after super '.' expression}}
  }
}

extension ThisBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  def baseExtFunc0() {}

  static def baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {
    init() { }
  }
  enum BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

extension ThisDerived1 {
  var derivedExtProp : Int {
  get:
    return 42
  set(val):
  }

  def derivedExtFunc0() {}

  static def derivedExtStaticFunc0() {}

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {
    init() { }
  }
  enum DerivedExtNestedUnion {
    case DerivedExtUnionX(Int)
  }

  typealias DerivedExtNestedTypealias = Int
}

// <rdar://problem/11554141>
def shadowbug() {
  var Foo = 10
  def g() {
    struct S {
      var x : Foo
      typealias Foo = Int
    }
  }
}
def scopebug() {
  var Foo = 10
  struct S {
    typealias Foo = Int
  }
}
struct Ordering {
  var x : Foo
  typealias Foo = Int
}

// <rdar://problem/12202655>
class Outer {
  class Inner {}
  class MoreInner : Inner {}
}

def makeGenericStruct<S>(x: S) -> GenericStruct<S> {
  return GenericStruct<S>()
}
struct GenericStruct<T> {}


// <rdar://problem/13952064>
extension Outer {
  class ExtInner {}
}

// <rdar://problem/14149537>
def useProto<R : MyProto>(val: R) -> R.Element {
  return val.get()
}

protocol MyProto {
  typealias Element
  def get() -> Element
}


// <rdar://problem/14488311>
struct DefaultArgumentFromExtension {
  def g(x: (@inout DefaultArgumentFromExtension) -> () -> () = f) {
    var f = 42
    var x2 = x
    x2 = f // expected-error{{not convertible}}
  }
  var x : (@inout DefaultArgumentFromExtension) -> () -> () = f
}
extension DefaultArgumentFromExtension {
  def f() {}
}
