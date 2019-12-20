// RUN: %target-typecheck-verify-swift -swift-version 5

@override // expected-error {{'override' can only be specified on class members}} {{1-11=}} expected-error {{'override' is a declaration modifier, not an attribute}} {{1-2=}}
func virtualAttributeCanNotBeUsedInSource() {}

class MixedKeywordsAndAttributes { // expected-note {{in declaration of 'MixedKeywordsAndAttributes'}}
  // expected-error@+1 {{expected declaration}} expected-error@+1 {{consecutive declarations on a line must be separated by ';'}} {{11-11=;}}
  override @inline(never) func f1() {}
}

class DuplicateOverrideBase {
  func f1() {}
  class func cf1() {}
  class func cf2() {}
  class func cf3() {}
  class func cf4() {}
}
class DuplicateOverrideDerived : DuplicateOverrideBase {
  override override func f1() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
  override override class func cf1() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
  override class override func cf2() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
  class override override func cf3() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
  override class override func cf4() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
}

class A {
  func f0() { }
  func f1() { } // expected-note{{overridden declaration is here}}

  var v1: Int { return 5 }
  var v2: Int { return 5 } // expected-note{{overridden declaration is here}}
  internal var v21: Int { return 5 } // expected-note{{overridden declaration is here}}
  var v4: String { return "hello" }// expected-note{{attempt to override property here}}
  var v5: A { return self }
  var v6: A { return self }
  var v7: A { // expected-note{{attempt to override property here}}
    get { return self }
    set { }
  }
  var v8: Int = 0  // expected-note {{attempt to override property here}}
  var v9: Int { return 5 } // expected-note{{attempt to override property here}}
  var v10: Int { return 5 } // expected-note{{attempt to override property here}}

  subscript (i: Int) -> String { // expected-note{{potential overridden subscript 'subscript(_:)' here}}
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-note{{overridden declaration is here}} expected-note{{potential overridden subscript 'subscript(_:)' here}}
    get {
      return "hello"
    }

    set {
    }
  }
  
  // FIXME(SR-10323): The second note is wrong; it should be "potential overridden class subscript 'subscript(_:)' here". This is a preexisting bug.
  class subscript (i: String) -> String { // expected-note{{overridden declaration is here}} expected-note{{attempt to override subscript here}}
    get {
      return "hello"
    }
    
    set {
    }
  }
  
  class subscript (typeInSuperclass a: [Int]) -> String {
    get {
      return "hello"
    }
    
    set {
    }
  }

  subscript (i: Int8) -> A { // expected-note{{potential overridden subscript 'subscript(_:)' here}}
    get { return self }
  }

  subscript (i: Int16) -> A { // expected-note{{attempt to override subscript here}} expected-note{{potential overridden subscript 'subscript(_:)' here}}
    get { return self }
    set { }
  }

  func overriddenInExtension() {} // expected-note {{overr}}
}

class B : A {
  override func f0() { }
  func f1() { } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}
  override func f2() { } // expected-error{{method does not override any method from its superclass}}
  override var v1: Int { return 5 }
  var v2: Int { return 5 } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}
  internal var v21: Int { return 5 } // expected-error{{overriding declaration requires an 'override' keyword}}{{12-12=override }}
  override var v3: Int { return 5 } // expected-error{{property does not override any property from its superclass}}
  override var v4: Int { return 5 } // expected-error{{property 'v4' with type 'Int' cannot override a property with type 'String'}}

  // Covariance
  override var v5: B { return self }
  override var v6: B {
    get { return self }
    set { }
  }

  override var v7: B { // expected-error{{cannot override mutable property 'v7' of type 'A' with covariant type 'B'}}
    get { return self }
    set { }
  }

  // Stored properties
  override var v8: Int { return 5 } // expected-error {{cannot override mutable property with read-only property 'v8'}}
  override var v9: Int // expected-error{{cannot override with a stored property 'v9'}}
  lazy override var v10: Int = 5 // expected-error{{cannot override with a stored property 'v10'}}

  override subscript (i: Int) -> String {
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-error{{overriding declaration requires an 'override' keyword}} {{3-3=override }}
    get {
      return "hello"
    }

    set {
    }
  }

  override subscript (f: Float) -> String { // expected-error{{subscript does not override any subscript from its superclass}}
    get {
      return "hello"
    }

    set {
    }
  }
  
  // FIXME(SR-10323): This error is wrong; it should be "subscript does not override any subscript from its superclass". This is a preexisting bug.
  override class subscript (i: Int) -> String { // expected-error{{cannot override mutable subscript of type '(Int) -> String' with covariant type '(String) -> String'}}
    get {
      return "hello"
    }
    
    set {
    }
  }
  
  static subscript (i: String) -> String { // expected-error{{overriding declaration requires an 'override' keyword}} {{10-10=override }}
    get {
      return "hello"
    }
    
    set {
    }
  }
  
  static subscript (i: Double) -> String {
    get {
      return "hello"
    }
    
    set {
    }
  }
  
  override class subscript (typeInSuperclass a: [Int]) -> String {
    get {
      return "hello"
    }
    
    set {
    }
  }

  override subscript (typeInSuperclass a: [Int]) -> String { // expected-error{{subscript does not override any subscript from its superclass}}
    get {
      return "hello"
    }

    set {
    }
  }

  // Covariant
  override subscript (i: Int8) -> B {
    get { return self }
  }

  override subscript (i: Int16) -> B { // expected-error{{cannot override mutable subscript of type '(Int16) -> B' with covariant type '(Int16) -> A'}}
    get { return self }
    set { }
  }

  override init() { }
  override deinit { } // expected-error{{'override' modifier cannot be applied to this declaration}} {{3-12=}}
  override typealias Inner = Int // expected-error{{'override' modifier cannot be applied to this declaration}} {{3-12=}}
}

extension B {
  override func overriddenInExtension() {} // expected-error{{overr}}
}

struct S {
  override func f() { } // expected-error{{'override' can only be specified on class members}} {{3-12=}}
}
extension S {
  override func ef() {} // expected-error{{method does not override any method from its superclass}}
}

enum E {
  override func f() { } // expected-error{{'override' can only be specified on class members}} {{3-12=}}
}

protocol P {
  override func f() // FIXME wording: expected-error{{method does not override any method from its superclass}}
}

override func f() { } // expected-error{{'override' can only be specified on class members}} {{1-10=}}

// Invalid 'override' on declarations inside closures.
var rdar16654075a = {
  override func foo() {}  // expected-error{{'override' can only be specified on class members}} {{3-12=}}
}
var rdar16654075b = {
  class A {
    override func foo() {}  // expected-error{{method does not override any method from its superclass}}
  }
}
var rdar16654075c = { () -> () in
  override func foo() {} // expected-error {{'override' can only be specified on class members}} {{3-12=}}
  ()
}
var rdar16654075d = { () -> () in
  class A {
    override func foo() {} // expected-error {{method does not override any method from its superclass}}
  }
  A().foo()
}
var rdar16654075e = { () -> () in
  class A {
    func foo() {}
  }
  class B : A {
    override func foo() {}
  }
  A().foo()
}

class C { 
  init(string: String) { } // expected-note{{overridden declaration is here}}
  required init(double: Double) { } // expected-note 3{{overridden required initializer is here}}

  convenience init() { self.init(string: "hello") } // expected-note{{attempt to override convenience initializer here}}
}

class D1 : C {
  override init(string: String) { super.init(string: string) }
  required init(double: Double) { }
  convenience init() { self.init(string: "hello") }
}

class D2 : C {
  init(string: String) { super.init(string: string) } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}

  // FIXME: Would like to remove the space after 'override' as well.
  required override init(double: Double) { } // expected-warning{{'override' is implied when overriding a required initializer}} {{12-21=}}
  override convenience init() { self.init(string: "hello") } // expected-error{{initializer does not override a designated initializer from its superclass}}
}

class D3 : C {
  override init(string: String) { super.init(string: string) }
  override init(double: Double) { } // expected-error{{use the 'required' modifier to override a required initializer}}{{3-11=required}}
}

class D4 : C {
  // "required override" only when we're overriding a non-required
  // designated initializer with a required initializer.
  required override init(string: String) { super.init(string: string) }
  required init(double: Double) { }
}

class D5 : C {
  // "required override" only when we're overriding a non-required
  // designated initializer with a required initializer.
  required convenience override init(string: String) { self.init(double: 5.0) }
  required init(double: Double) { }
}

class D6 : C {
  init(double: Double) { } // expected-error{{'required' modifier must be present on all overrides of a required initializer}} {{3-3=required }}
}

// rdar://problem/18232867
class C_empty_tuple {
  init() { }
}

class D_empty_tuple : C_empty_tuple {
  override init(foo:()) { } // expected-error{{initializer does not override a designated initializer from its superclass}}
}

class C_with_let {
  let x = 42  // expected-note {{attempt to override property here}}
}

class D_with_let : C_with_let {
  override var x : Int { get { return 4 } set {} }  // expected-error {{cannot override immutable 'let' property 'x' with the getter of a 'var'}}
}


// <rdar://problem/21311590> QoI: Inconsistent diagnostics when no constructor is available
class C21311590 {
  override init() {}  // expected-error {{initializer does not override a designated initializer from its superclass}}
}
class B21311590 : C21311590 {}
_ = C21311590()
_ = B21311590()


class MismatchOptionalBase {
  func param(_: Int?) {}
  func paramIUO(_: Int!) {}
  func result() -> Int { return 0 }

  func fixSeveralTypes(a: Int?, b: Int!) -> Int { return 0 }

  func functionParam(x: ((Int) -> Int)?) {}
  func tupleParam(x: (Int, Int)?) {}
  func compositionParam(x: (P1 & P2)?) {}

  func nameAndTypeMismatch(label: Int?) {}

  func ambiguousOverride(a: Int, b: Int?) {} // expected-note 2 {{overridden declaration is here}} expected-note {{potential overridden instance method 'ambiguousOverride(a:b:)' here}}
  func ambiguousOverride(a: Int?, b: Int) {} // expected-note 2 {{overridden declaration is here}} expected-note {{potential overridden instance method 'ambiguousOverride(a:b:)' here}}

  var prop: Int = 0 // expected-note {{attempt to override property here}}
  var optProp: Int? // expected-note {{attempt to override property here}}

  var getProp: Int { return 0 } // expected-note {{attempt to override property here}}
  var getOptProp: Int? { return nil }

  init(param: Int?) {}
  init() {} // expected-note {{non-failable initializer 'init()' overridden here}}

  subscript(a: Int?) -> Void { // expected-note {{attempt to override subscript here}}
    get { return () }
    set {}
  }
  subscript(b: Void) -> Int { // expected-note {{attempt to override subscript here}}
    get { return 0 }
    set {}
  }

  subscript(get a: Int?) -> Void {
    return ()
  }
  subscript(get b: Void) -> Int {
    return 0
  }

  subscript(ambiguous a: Int, b: Int?) -> Void { // expected-note {{overridden declaration is here}} expected-note {{potential overridden subscript 'subscript(ambiguous:_:)' here}}
    return ()
  }
  subscript(ambiguous a: Int?, b: Int) -> Void { // expected-note {{overridden declaration is here}} expected-note {{potential overridden subscript 'subscript(ambiguous:_:)' here}}
    return ()
  }
}

protocol P1 {}
protocol P2 {}

class MismatchOptional : MismatchOptionalBase {
  override func param(_: Int) {} // expected-error {{cannot override instance method parameter of type 'Int?' with non-optional type 'Int'}} {{29-29=?}}
  override func paramIUO(_: Int) {} // expected-error {{cannot override instance method parameter of type 'Int?' with non-optional type 'Int'}} {{32-32=?}}
  override func result() -> Int? { return nil } // expected-error {{cannot override instance method result type 'Int' with optional type 'Int?'}} {{32-33=}}

  override func fixSeveralTypes(a: Int, b: Int) -> Int! { return nil }
  // expected-error@-1 {{cannot override instance method parameter of type 'Int?' with non-optional type 'Int'}} {{39-39=?}}
  // expected-error@-2 {{cannot override instance method parameter of type 'Int?' with non-optional type 'Int'}} {{47-47=?}}
  // expected-error@-3 {{cannot override instance method result type 'Int' with optional type 'Int?'}} {{55-56=}}

  override func functionParam(x: @escaping (Int) -> Int) {} // expected-error {{cannot override instance method parameter of type '((Int) -> Int)?' with non-optional type '(Int) -> Int'}} {{34-34=(}} {{56-56=)?}}
  override func tupleParam(x: (Int, Int)) {} // expected-error {{cannot override instance method parameter of type '(Int, Int)?' with non-optional type '(Int, Int)'}} {{41-41=?}}
  override func compositionParam(x: P1 & P2) {} // expected-error {{cannot override instance method parameter of type '(P1 & P2)?' with non-optional type 'P1 & P2'}} {{37-37=(}} {{44-44=)?}}

  override func nameAndTypeMismatch(_: Int) {}
  // expected-error@-1 {{argument labels for method 'nameAndTypeMismatch' do not match those of overridden method 'nameAndTypeMismatch(label:)'}} {{37-37=label }}
  // expected-error@-2 {{cannot override instance method parameter of type 'Int?' with non-optional type 'Int'}} {{43-43=?}}

  override func ambiguousOverride(a: Int?, b: Int?) {} // expected-error {{declaration 'ambiguousOverride(a:b:)' cannot override more than one superclass declaration}} {{none}}
  override func ambiguousOverride(a: Int, b: Int) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  override var prop: Int? { // expected-error {{property 'prop' with type 'Int?' cannot override a property with type 'Int'}} {{none}}
    get { return nil }
    set {}
  }
  override var optProp: Int { // expected-error {{cannot override mutable property 'optProp' of type 'Int?' with covariant type 'Int'}} {{none}}
    get { return 0 }
    set {}
  }
  override var getProp: Int? { return nil } // expected-error {{property 'getProp' with type 'Int?' cannot override a property with type 'Int'}} {{none}}
  override var getOptProp: Int { return 0 } // okay

  override init(param: Int) {} // expected-error {{cannot override initializer parameter of type 'Int?' with non-optional type 'Int'}}
  override init?() {} // expected-error {{failable initializer 'init()' cannot override a non-failable initializer}} {{none}}

  override subscript(a: Int) -> Void { // expected-error {{cannot override mutable subscript of type '(Int) -> Void' with covariant type '(Int?) -> Void'}}
    get { return () }
    set {}
  }
  override subscript(b: Void) -> Int? { // expected-error {{cannot override mutable subscript of type '(Void) -> Int?' with covariant type '(Void) -> Int'}}
    get { return nil }
    set {}
  }

  override subscript(get a: Int) -> Void { // expected-error {{cannot override subscript index of type 'Int?' with non-optional type 'Int'}} {{32-32=?}}
    return ()
  }
  override subscript(get b: Void) -> Int? { // expected-error {{cannot override subscript element type 'Int' with optional type 'Int?'}} {{41-42=}}
    return nil
  }

  override subscript(ambiguous a: Int?, b: Int?) -> Void { // expected-error {{declaration 'subscript(ambiguous:_:)' cannot override more than one superclass declaration}}
    return ()
  }
  override subscript(ambiguous a: Int, b: Int) -> Void { // expected-error {{subscript does not override any subscript from its superclass}}
    return ()
  }
}

class MismatchOptional2 : MismatchOptionalBase {
  override func result() -> Int! { return nil } // expected-error {{cannot override instance method result type 'Int' with optional type 'Int?'}} {{32-33=}}

  // None of these are overrides because we didn't say 'override'. Since they're
  // not exact matches, they shouldn't result in errors.
  func param(_: Int) {}
  func ambiguousOverride(a: Int, b: Int) {}

  // This is covariant, so we still assume it's meant to override.
  func ambiguousOverride(a: Int?, b: Int?) {} // expected-error {{declaration 'ambiguousOverride(a:b:)' cannot override more than one superclass declaration}}
}

class MismatchOptional3 : MismatchOptionalBase {
  override func result() -> Optional<Int> { return nil } // expected-error {{cannot override instance method result type 'Int' with optional type 'Optional<Int>'}} {{none}}
}

// Make sure we remap the method's innermost generic parameters
// to the correct depth
class GenericBase<T> {
  func doStuff<U>(t: T, u: U) {}
  init<U>(t: T, u: U) {}
}

class ConcreteSub : GenericBase<Int> {
  override func doStuff<U>(t: Int, u: U) {}
  override init<U>(t: Int, u: U) {}
}

class ConcreteBase {
  init<U>(t: Int, u: U) {}
  func doStuff<U>(t: Int, u: U) {}

}

class GenericSub<T> : ConcreteBase {
  override init<U>(t: Int, u: U) {}
  override func doStuff<U>(t: Int, u: U) {}
}

// Issue with generic parameter index
class MoreGenericSub1<T, TT> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {}
}

class MoreGenericSub2<TT, T> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {}
}

// Issue with insufficient canonicalization when
// comparing types
protocol SI {}
protocol CI {}

protocol Sequence {
  associatedtype I : SI // expected-note{{declared here}}
}

protocol Collection : Sequence {
  associatedtype I : CI // expected-warning{{redeclaration of associated type 'I'}}
}

class Index<F, T> {
  func map(_ f: F) -> T {}
}

class CollectionIndex<C : Collection> : Index<C, C.I> {
  override func map(_ f: C) -> C.I {}
}

// SR-4206: Overrides with different generic signature

// Base class is generic, derived class is concrete //

protocol SR_4206_Protocol_1 {}
protocol SR_4206_Protocol_2 {}

class SR_4206_BaseGeneric_1<T> {
  func foo<T: SR_4206_Protocol_1>(arg: T) {}
}

class SR_4206_DerivedConcrete_1: SR_4206_BaseGeneric_1<SR_4206_Protocol_2> {
  override func foo<T>(arg: T) {} // Ok?
}

// Base class is concrete, derived class is generic //

class SR_4206_BaseConcrete_1 {
  func foo() {}
}

class SR_4206_DerivedGeneric_1<T>: SR_4206_BaseConcrete_1 {
  override func foo<T>(arg: T) {} // expected-error {{method does not override any method from its superclass}}
}

// Base class generic w/ method generic, derived class generic w/ method not generic

class SR_4206_BaseGeneric_2<T> {
  func foo<T>(arg: T) {}
}

class SR_4206_DerivedConcrete_2<T>: SR_4206_BaseGeneric_2<T> {
  override func foo() {} // expected-error {{method does not override any method from its superclass}}
}

// Base class generic w/ method generic, derived class generic w/ method generic but different requirement

class SR_4206_BaseGeneric_3<T> {
  func foo<T>(arg: T) {} // expected-note {{overridden declaration is here}}
}

class SR_4206_DerivedGeneric_3<T>: SR_4206_BaseGeneric_3<T> {
  override func foo<T: SR_4206_Protocol_1>(arg: T) {} // expected-error {{overridden method 'foo' has generic signature <T, T where T : SR_4206_Protocol_1> which is incompatible with base method's generic signature <T, T>; expected generic signature to be <T, T>}}
}

// Base class not generic w/ method generic, derived class not generic w/ method generic but different requirement

class SR_4206_BaseConcrete_4 {
  func foo<T>(arg: T) {} // expected-note {{overridden declaration is here}}
}

class SR_4206_DerivedConcrete_4: SR_4206_BaseConcrete_4 {
  override func foo<T: SR_4206_Protocol_1>(arg: T) {} // expected-error {{overridden method 'foo' has generic signature <T where T : SR_4206_Protocol_1> which is incompatible with base method's generic signature <T>; expected generic signature to be <T>}}
}

// Base class not generic w/ method generic, derived class not generic w/ method generic but removed requirement

class SR_4206_BaseConcrete_5 {
  func foo<T: SR_4206_Protocol_2>(arg: T) {}
}

class SR_4206_DerivedConcrete_5: SR_4206_BaseConcrete_5 {
  override func foo<T>(arg: T) {} // Ok?
}

// Base class not generic w/ method generic, derived class generic w/ method generic but different requirement

class SR_4206_BaseConcrete_6 {
  func foo<T: SR_4206_Protocol_2>(arg: T) {} // expected-note {{overridden declaration is here}}
}

class SR_4206_DerivedGeneric_6<T>: SR_4206_BaseConcrete_6 {
  override func foo<T: SR_4206_Protocol_1>(arg: T) {} // expected-error {{overridden method 'foo' has generic signature <T, T where T : SR_4206_Protocol_1> which is incompatible with base method's generic signature <T where T : SR_4206_Protocol_2>; expected generic signature to be <T, T where T : SR_4206_Protocol_2>}}
}

// Misc //

protocol SR_4206_Key {}

protocol SR_4206_Container {
  associatedtype Key: SR_4206_Key
}

class SR_4206_Base<Key: SR_4206_Key> {
  func foo(forKey key: Key) throws {}
}

class SR_4206_Derived<C: SR_4206_Container> : SR_4206_Base<C.Key> {
  typealias Key = C.Key
  override func foo(forKey key: Key) throws {} // Okay, no generic signature mismatch
}

// SR-10198

class SR_10198_Base {
  func a<T>(_ val: T) -> String { return "not equatable" }
  func a<T: Equatable>(_ val: T) -> String { return "equatable" }
}

class SR_10198_Derived: SR_10198_Base {
  override func a<T>(_ val: T) -> String { return super.a(val) } // okay
  override func a<T: Equatable>(_ val: T) -> String { return super.a(val) } // okay
}

protocol SR_10198_Base_P {
  associatedtype Bar
}

struct SR_10198_Base_S: SR_10198_Base_P {
  typealias Bar = Int
}

class SR_10198_Base_1 {
  init<F: SR_10198_Base_P>(_ arg: F) where F.Bar == Int {}
}

class SR_10198_Derived_1: SR_10198_Base_1 {
  init(_ arg1: Int) { super.init(SR_10198_Base_S()) } // okay, doesn't crash
}

// SR-11740

public class SR_11740_Base<F, A> {}

public class SR_11740_Derived<F, A>
  : SR_11740_Base<SR_11740_Base<F, A>, A>,
    SR_11740_Q {}

public protocol SR_11740_P {}

public protocol SR_11740_Q: SR_11740_P {
    associatedtype A
}

public extension SR_11740_Base where F: SR_11740_Q {
    static func foo(_: F.A) {}
}

extension SR_11740_Derived where F: SR_11740_P {
    public static func foo(_: A) {}
}

