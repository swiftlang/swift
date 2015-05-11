// RUN: %target-parse-verify-swift

class ThisBase1 {
  init() { }

  var baseInstanceVar: Int

  var baseProp : Int {
    get {
      return 42
    }
    set {}
  }

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set {
      baseInstanceVar = i
    }
  }

  class var baseStaticVar: Int = 42 // expected-error {{class stored properties not yet supported}}

  class var baseStaticProp: Int {
    get {
      return 42
    }
    set {}
  }

  class func baseStaticFunc0() {}

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
  override init() { super.init() }

  var derivedInstanceVar: Int

  var derivedProp : Int {
    get {
      return 42
    }
    set {}
  }

  func derivedFunc0() {}
  func derivedFunc1(a: Int) {}

  subscript(i: Double) -> Int {
    get {
      return Int(i)
    }
    set {
      baseInstanceVar = Int(i)
    }
  }

  class var derivedStaticVar: Int = 42// expected-error {{class stored properties not yet supported}}

  class var derivedStaticProp: Int {
    get {
      return 42
    }
    set {}
  }

  class func derivedStaticFunc0() {}

  struct DerivedNestedStruct {}
  class DerivedNestedClass {
    init() { }
  }
  enum DerivedNestedUnion {
    case DerivedUnionX(Int)
  }

  typealias DerivedNestedTypealias = Int

  func testSelf1() {
    self.baseInstanceVar = 42
    self.baseProp = 42
    self.baseFunc0()
    self.baseFunc1(42)
    self[0] = 42.0
    self.baseStaticVar = 42 // expected-error {{'ThisDerived1' does not have a member named 'baseStaticVar'}}
    self.baseStaticProp = 42 // expected-error {{'ThisDerived1' does not have a member named 'baseStaticProp'}}
    self.baseStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseStaticFunc0'}}

    self.baseExtProp = 42
    self.baseExtFunc0()
    self.baseExtStaticVar = 42 // expected-error {{'ThisDerived1' does not have a member named 'baseExtStaticVar'}}
    self.baseExtStaticProp = 42
    self.baseExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'baseExtStaticFunc0'}}

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedUnion = .BaseUnionX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = self.BaseNestedStruct() // expected-error{{'ThisDerived1' does not have a member named 'BaseNestedStruct'}}
    var bc2 = self.BaseNestedClass() // expected-error{{'ThisDerived1' does not have a member named 'BaseNestedClass'}}
    var bo2 = self.BaseUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseUnionX'}}
    var bo3 = self.BaseNestedUnion.BaseUnionX(24) // expected-error{{'ThisDerived1' does not have a member named 'BaseNestedUnion'}}
    var bt2 = self.BaseNestedTypealias(42) // expected-error{{'ThisDerived1' does not have a member named 'BaseNestedTypealias'}}

    var bes1 : BaseExtNestedStruct
    var bec1 : BaseExtNestedClass
    var beo1 : BaseExtNestedUnion = .BaseExtUnionX(42)
    var bet1 : BaseExtNestedTypealias
    var bes2 = self.BaseExtNestedStruct() // expected-error{{ThisDerived1' does not have a member named 'BaseExtNestedStruct'}}
    var bec2 = self.BaseExtNestedClass() // expected-error{{ThisDerived1' does not have a member named 'BaseExtNestedClass'}}
    var beo2 = self.BaseExtUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'BaseExtUnionX'}}
    var beo3 = self.BaseExtNestedUnion.BaseExtUnionX(24) // expected-error{{ThisDerived1' does not have a member named 'BaseExtNestedUnion'}}
    var bet2 = self.BaseExtNestedTypealias(42) // expected-error{{ThisDerived1' does not have a member named 'BaseExtNestedTypealias'}}

    self.derivedInstanceVar = 42
    self.derivedProp = 42
    self.derivedFunc0()
    self.derivedStaticVar = 42 // expected-error {{'ThisDerived1' does not have a member named 'derivedStaticVar'}}
    self.derivedStaticProp = 42 // expected-error {{'ThisDerived1' does not have a member named 'derivedStaticProp'}}
    self.derivedStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedStaticFunc0'}}

    self.derivedExtProp = 42
    self.derivedExtFunc0()
    self.derivedExtStaticVar = 42 // expected-error {{'ThisDerived1' does not have a member named 'derivedExtStaticVar'}}
    self.derivedExtStaticProp = 42
    self.derivedExtStaticFunc0() // expected-error {{'ThisDerived1' does not have a member named 'derivedExtStaticFunc0'}}

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedUnion = .DerivedUnionX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = self.DerivedNestedStruct() // expected-error{{ThisDerived1' does not have a member named 'DerivedNestedStruct'}}
    var dc2 = self.DerivedNestedClass() // expected-error{{ThisDerived1' does not have a member named 'DerivedNestedClass'}}
    var do2 = self.DerivedUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedUnionX'}}
    var do3 = self.DerivedNestedUnion.DerivedUnionX(24) // expected-error{{ThisDerived1' does not have a member named 'DerivedNestedUnion'}}
    var dt2 = self.DerivedNestedTypealias(42) // expected-error{{ThisDerived1' does not have a member named 'DerivedNestedTypealias'}}

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedUnion = .DerivedExtUnionX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = self.DerivedExtNestedStruct() // expected-error{{ThisDerived1' does not have a member named 'DerivedExtNestedStruct'}}
    var dec2 = self.DerivedExtNestedClass() // expected-error{{ThisDerived1' does not have a member named 'DerivedExtNestedClass'}}
    var deo2 = self.DerivedExtUnionX(24) // expected-error {{'ThisDerived1' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = self.DerivedExtNestedUnion.DerivedExtUnionX(24) // expected-error{{ThisDerived1' does not have a member named 'DerivedExtNestedUnion'}}
    var det2 = self.DerivedExtNestedTypealias(42) // expected-error{{ThisDerived1' does not have a member named 'DerivedExtNestedTypealias'}}

    self.Type // expected-error {{does not have a member named 'Type'}}
  }

  func testSuper1() {
    super.baseInstanceVar = 42
    super.baseProp = 42
    super.baseFunc0()
    super.baseFunc1(42)
    super[0] = 42.0
    super.baseStaticVar = 42 // expected-error {{'ThisBase1' does not have a member named 'baseStaticVar'}}
    super.baseStaticProp = 42 // expected-error {{'ThisBase1' does not have a member named 'baseStaticProp'}}
    super.baseStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'baseStaticFunc0'}}

    super.baseExtProp = 42
    super.baseExtFunc0()
    super.baseExtStaticVar = 42 // expected-error {{'ThisBase1' does not have a member named 'baseExtStaticVar'}}
    super.baseExtStaticProp = 42
    super.baseExtStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'baseExtStaticFunc0'}}

    var bs2 = super.BaseNestedStruct() // expected-error{{'ThisBase1' does not have a member named 'BaseNestedStruct'}}
    var bc2 = super.BaseNestedClass() // expected-error{{'ThisBase1' does not have a member named 'BaseNestedClass'}}
    var bo2 = super.BaseUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'BaseUnionX'}}
    var bo3 = super.BaseNestedUnion.BaseUnionX(24) // expected-error{{'ThisBase1' does not have a member named 'BaseNestedUnion'}}
    var bt2 = super.BaseNestedTypealias(42) // expected-error{{'ThisBase1' does not have a member named 'BaseNestedTypealias'}}

    var bes2 = super.BaseExtNestedStruct() // expected-error{{'ThisBase1' does not have a member named 'BaseExtNestedStruct'}}
    var bec2 = super.BaseExtNestedClass() // expected-error{{'ThisBase1' does not have a member named 'BaseExtNestedClass'}}
    var beo2 = super.BaseExtUnionX(24) // expected-error {{'ThisBase1' does not have a member named 'BaseExtUnionX'}}
    var beo3 = super.BaseExtNestedUnion.BaseExtUnionX(24) // expected-error{{'ThisBase1' does not have a member named 'BaseExtNestedUnion'}}
    var bet2 = super.BaseExtNestedTypealias(42) // expected-error{{'ThisBase1' does not have a member named 'BaseExtNestedTypealias'}}

    super.derivedInstanceVar = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedInstanceVar'}}
    super.derivedProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedProp'}}
    super.derivedFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedFunc0'}}
    super.derivedStaticVar = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedStaticVar'}}
    super.derivedStaticProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedStaticProp'}}
    super.derivedStaticFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedStaticFunc0'}}

    super.derivedExtProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedExtProp'}}
    super.derivedExtFunc0() // expected-error {{'ThisBase1' does not have a member named 'derivedExtFunc0'}}
    super.derivedExtStaticVar = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedExtStaticVar'}}
    super.derivedExtStaticProp = 42 // expected-error {{'ThisBase1' does not have a member named 'derivedExtStaticProp'}}
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

    super.Type // expected-error {{does not have a member named 'Type'}}
  }

  class func staticTestSelf1() {
    self.baseInstanceVar = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'baseInstanceVar'}}
    self.baseProp = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'baseProp'}}
    self.baseFunc0() // expected-error {{missing argument}}
    self.baseFunc0(ThisBase1())()
    self.baseFunc1(42) // expected-error {{cannot invoke 'baseFunc1' with an argument list of type '(Int)'}}
    self.baseFunc1(ThisBase1())(42)
    self[0] = 42.0 // expected-error {{'ThisDerived1.Type' does not have a member named 'subscript'}}
    self.baseStaticVar = 42
    self.baseStaticProp = 42
    self.baseStaticFunc0()

    self.baseExtProp = 42 // expected-error {{ThisDerived1.Type' does not have a member named 'baseExtProp'}}
    self.baseExtFunc0() // expected-error {{missing argument}}
    self.baseExtStaticVar = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'baseExtStaticVar'}}
    self.baseExtStaticProp = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'baseExtStaticProp'}}
    self.baseExtStaticFunc0()

    var bs1 : BaseNestedStruct
    var bc1 : BaseNestedClass
    var bo1 : BaseNestedUnion = .BaseUnionX(42)
    var bt1 : BaseNestedTypealias
    var bs2 = self.BaseNestedStruct()
    var bc2 = self.BaseNestedClass()
    var bo2 = self.BaseUnionX(24) // expected-error {{'ThisDerived1.Type' does not have a member named 'BaseUnionX'}}
    var bo3 = self.BaseNestedUnion.BaseUnionX(24)
    var bt2 = self.BaseNestedTypealias()

    self.derivedInstanceVar = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'derivedInstanceVar'}}
    self.derivedProp = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'derivedProp'}}
    self.derivedFunc0() // expected-error {{missing argument}}
    self.derivedFunc0(ThisBase1())() // expected-error {{unable to infer closure type in the current context}}
    self.derivedFunc0(ThisDerived1())()
    self.derivedStaticVar = 42
    self.derivedStaticProp = 42
    self.derivedStaticFunc0()

    self.derivedExtProp = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'derivedExtProp'}}
    self.derivedExtFunc0() // expected-error {{missing argument}}
    self.derivedExtStaticVar = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'derivedExtStaticVar'}}
    self.derivedExtStaticProp = 42 // expected-error {{'ThisDerived1.Type' does not have a member named 'derivedExtStaticProp'}}
    self.derivedExtStaticFunc0()

    var ds1 : DerivedNestedStruct
    var dc1 : DerivedNestedClass
    var do1 : DerivedNestedUnion = .DerivedUnionX(42)
    var dt1 : DerivedNestedTypealias
    var ds2 = self.DerivedNestedStruct()
    var dc2 = self.DerivedNestedClass()
    var do2 = self.DerivedUnionX(24) // expected-error {{'ThisDerived1.Type' does not have a member named 'DerivedUnionX'}}
    var do3 = self.DerivedNestedUnion.DerivedUnionX(24)
    var dt2 = self.DerivedNestedTypealias()

    var des1 : DerivedExtNestedStruct
    var dec1 : DerivedExtNestedClass
    var deo1 : DerivedExtNestedUnion = .DerivedExtUnionX(42)
    var det1 : DerivedExtNestedTypealias
    var des2 = self.DerivedExtNestedStruct()
    var dec2 = self.DerivedExtNestedClass()
    var deo2 = self.DerivedExtUnionX(24) // expected-error {{'ThisDerived1.Type' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = self.DerivedExtNestedUnion.DerivedExtUnionX(24)
    var det2 = self.DerivedExtNestedTypealias()

    self.Type // expected-error {{does not have a member named 'Type'}}
  }

  class func staticTestSuper1() {
    super.baseInstanceVar = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'baseInstanceVar'}}
    super.baseProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'baseProp'}}
    super.baseFunc0() // expected-error {{missing argument}}
    super.baseFunc0(ThisBase1())()
    super.baseFunc1(42) // expected-error {{cannot invoke 'baseFunc1' with an argument list of type '(Int)'}}
    super.baseFunc1(ThisBase1())(42)
    super[0] = 42.0 // expected-error {{'ThisBase1.Type' does not have a member named 'subscript'}}
    super.baseStaticVar = 42
    super.baseStaticProp = 42
    super.baseStaticFunc0()

    super.baseExtProp = 42 // expected-error {{ThisBase1.Type' does not have a member named 'baseExtProp'}}
    super.baseExtFunc0() // expected-error {{missing argument}}
    super.baseExtStaticVar = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'baseExtStaticVar'}}
    super.baseExtStaticProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'baseExtStaticProp'}}
    super.baseExtStaticFunc0()

    var bs2 = super.BaseNestedStruct()
    var bc2 = super.BaseNestedClass()
    var bo2 = super.BaseUnionX(24) // expected-error {{'ThisBase1.Type' does not have a member named 'BaseUnionX'}}
    var bo3 = super.BaseNestedUnion.BaseUnionX(24)
    var bt2 = super.BaseNestedTypealias()

    super.derivedInstanceVar = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedInstanceVar'}}
    super.derivedProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedProp'}}
    super.derivedFunc0() // expected-error {{'ThisBase1.Type' does not have a member named 'derivedFunc0'}}
    super.derivedStaticVar = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedStaticVar'}}
    super.derivedStaticProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedStaticProp'}}
    super.derivedStaticFunc0() // expected-error {{'ThisBase1.Type' does not have a member named 'derivedStaticFunc0'}}

    super.derivedExtProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedExtProp'}}
    super.derivedExtFunc0() // expected-error {{'ThisBase1.Type' does not have a member named 'derivedExtFunc0'}}
    super.derivedExtStaticVar = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedExtStaticVar'}}
    super.derivedExtStaticProp = 42 // expected-error {{'ThisBase1.Type' does not have a member named 'derivedExtStaticProp'}}
    super.derivedExtStaticFunc0() // expected-error {{'ThisBase1.Type' does not have a member named 'derivedExtStaticFunc0'}}

    var ds2 = super.DerivedNestedStruct() // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedNestedStruct'}}
    var dc2 = super.DerivedNestedClass() // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedNestedClass'}}
    var do2 = super.DerivedUnionX(24) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedUnionX'}}
    var do3 = super.DerivedNestedUnion.DerivedUnionX(24) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedNestedUnion'}}
    var dt2 = super.DerivedNestedTypealias(42) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedNestedTypealias'}}

    var des2 = super.DerivedExtNestedStruct() // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedExtNestedStruct'}}
    var dec2 = super.DerivedExtNestedClass() // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedExtNestedClass'}}
    var deo2 = super.DerivedExtUnionX(24) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedExtUnionX'}}
    var deo3 = super.DerivedExtNestedUnion.DerivedExtUnionX(24) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedExtNestedUnion'}}
    var det2 = super.DerivedExtNestedTypealias(42) // expected-error {{'ThisBase1.Type' does not have a member named 'DerivedExtNestedTypealias'}}

    super.Type // expected-error {{does not have a member named 'Type'}}
  }
}

extension ThisBase1 {
  var baseExtProp : Int {
    get {
      return 42
    }
    set {}
  }

  func baseExtFunc0() {}

  var baseExtStaticVar: Int // expected-error {{extensions may not contain stored properties}}

  var baseExtStaticProp: Int {
    get {
      return 42
    }
    set {}
  }

  class func baseExtStaticFunc0() {}

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
    get {
      return 42
    }
    set {}
  }

  func derivedExtFunc0() {}

  var derivedExtStaticVar: Int // expected-error {{extensions may not contain stored properties}}

  var derivedExtStaticProp: Int {
    get {
      return 42
    }
    set {}
  }

  class func derivedExtStaticFunc0() {}

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
func shadowbug() {
  var Foo = 10
  func g() {
    struct S {
      var x : Foo
      typealias Foo = Int
    }
  }
}
func scopebug() {
  let Foo = 10
  struct S {
    typealias Foo = Int
  }
  _ = Foo
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

func makeGenericStruct<S>(x: S) -> GenericStruct<S> {
  return GenericStruct<S>()
}
struct GenericStruct<T> {}


// <rdar://problem/13952064>
extension Outer {
  class ExtInner {}
}

// <rdar://problem/14149537>
func useProto<R : MyProto>(value: R) -> R.Element {
  return value.get()
}

protocol MyProto {
  typealias Element
  func get() -> Element
}


// <rdar://problem/14488311>
struct DefaultArgumentFromExtension {
  func g(x: (DefaultArgumentFromExtension) -> () -> () = f) {
    let f = 42
    var x2 = x
    x2 = f // expected-error{{cannot assign a value of type 'Int' to a value of type '(DefaultArgumentFromExtension) -> () -> ()'}}
    _ = x2
  }
  var x : (DefaultArgumentFromExtension) -> () -> () = f
}
extension DefaultArgumentFromExtension {
  func f() {}
}

struct MyStruct {
  var state : Bool
  init() { state = true }
  mutating func mod() {state = false}
  func foo() { mod() } // expected-error {{immutable value of type 'MyStruct' only has mutating members named 'mod'}}
}

