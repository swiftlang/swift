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
  
  class subscript (i: String) -> String { // expected-note{{overridden declaration is here}} expected-note{{potential overridden class subscript 'subscript(_:)' here}}
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
  
  override class subscript (i: Int) -> String { // expected-error{{subscript does not override any subscript from its superclass}}
    get {
      return "hello"
    }
    
    set {
    }
  }
  
  static subscript (i: String) -> String { // expected-error{{overriding declaration requires an 'override' keyword}} {{3-3=override }}
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
  override func ef() {} // expected-error{{'override' can only be specified on class members}} {{3-12=}}
}

enum E {
  override func f() { } // expected-error{{'override' can only be specified on class members}} {{3-12=}}
}

protocol P {
  override func f() // expected-error{{method does not override any method from its parent protocol}}
  override var g: Int { get } // expected-error{{property does not override any property from its parent protocol}}
  override subscript(h: Int) -> Bool { get } // expected-error{{subscript does not override any subscript from its parent protocol}}
  override init(i: Int) // expected-error{{initializer does not override a designated initializer from its parent protocol}}
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
  override func compositionParam(x: P1 & P2) {} // expected-error {{cannot override instance method parameter of type '(any P1 & P2)?' with non-optional type 'any P1 & P2'}} {{37-37=(}} {{44-44=)?}}

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

// https://github.com/apple/swift/issues/46789
// Overrides with different generic signatures

protocol Protocol1 {}
protocol Protocol2 {}

// Base class is generic, derived class is concrete.

class Base1<T> {
  func foo<U: Protocol1>(arg: U) {}
}
class Derived1: Base1<Protocol2> {
  override func foo<T>(arg: T) {} // Ok?
}

// Base class is concrete, derived class is generic.

class Base2 {
  func foo() {}
}
class Derived2<T>: Base2 {
  override func foo<U>(arg: U) {} // expected-error {{method does not override any method from its superclass}}
}

// Base class generic w/ method generic, derived class generic w/ method not
// generic.

class Base3<T> {
  func foo<U>(arg: U) {}
}
class Derived3<T>: Base3<T> {
  override func foo() {} // expected-error {{method does not override any method from its superclass}}
}

// Base class generic w/ method generic, derived class generic w/ method generic
// but different requirement.

class Base4<T> {
  func foo<U>(arg: U) {}
}
class Derived4<T>: Base4<T> {
  override func foo<U: Protocol1>(arg: U) {} // expected-error {{method does not override any method from its superclass}}
}

// Base class not generic w/ method generic, derived class not generic w/ method
// generic but different requirement.

class Base5 {
  func foo<T>(arg: T) {}
}
class Derived5: Base5 {
  override func foo<T: Protocol1>(arg: T) {} // expected-error {{method does not override any method from its superclass}}
}

// Base class not generic w/ method generic, derived class not generic w/ method
// generic but removed requirement.

class Base6 {
  func foo<T: Protocol2>(arg: T) {}
}
class Derived6: Base6 {
  override func foo<T>(arg: T) {} // Ok?
}

// Base class not generic w/ method generic, derived class generic w/ method
// generic but different requirement.

class Base7 {
  func foo<T: Protocol2>(arg: T) {}
}
class Derived7<T>: Base7 {
  override func foo<U: Protocol1>(arg: U) {} // expected-error {{method does not override any method from its superclass}}
}

// Override with orthogonal requirement on contextual generic parameter.

class Base8<T> {
  func foo1() where T: Protocol1 {}
  func foo2() where T: Protocol1 {}
}
class Derived8<T>: Base8<T> {
  override func foo1() where T: Protocol2 {} // expected-error {{method does not override any method from its superclass}}

  override func foo2() {} // OK
}

// Subclass with new conformance requirement on inherited generic parameter.

class Base9<T> {
  func foo() where T: Protocol1 {}
}
class Derived9<T: Protocol2, U>: Base9<T> {
  // Because the generic signature of foo() is the same either way,
  // it may seem confusing that placing an additional constraint on the
  // generic parameter declaration directly has a different effect on
  // overridability in contrast to placing the constraint on foo().
  // The former (unlike the latter) is accepted because the constraint
  // in question only affects the ability to initialize an instance of the
  // subclass — not the visibility of the override itself relative to an
  // existing instance.
  override func foo() where T: Protocol1 {} // OK
}

// Override with less strict requirement via contextual where clauses.

class Base10<T> {
  func foo() where T == Int {}
}
class Derived10<T>: Base10<T> {
  override func foo() where T: FixedWidthInteger {} // OK
}

// Override with equivalent constraint on a different, non-inherited generic
// parameter.

class Base11<T> {
  func foo() where T: Protocol1 {}
}
class Derived11<T, U>: Base11<T> {
  override func foo() where U: Protocol1 {} // expected-error {{method does not override any method from its superclass}}
}

// Generic superclass / non-generic subclass.

class Base12<T> {
  // The fact that the return type matches the substitution
  // for T must hold across overrides.
  func foo() -> T where T: FixedWidthInteger { fatalError() } // expected-note {{potential overridden instance method 'foo()' here}}
}
class Derived12_1: Base12<Int> {
  override func foo() -> Int { return .zero } // OK
}
class Derived12_2: Base12<Bool> {
  override func foo() -> Int { return .zero } // expected-error {{method does not override any method from its superclass}}
}

// Misc //

protocol P_Key {}
protocol P_Container {
  associatedtype Key: P_Key
}

class Base13<Key: P_Key> {
  func foo(forKey key: Key) throws {}
}

class Derived13<C: P_Container> : Base13<C.Key> {
  typealias Key = C.Key
  override func foo(forKey key: Key) throws {} // Okay, no generic signature mismatch
}

// https://github.com/apple/swift/issues/52598

class Base1_52598 {
  func a<T>(_ val: T) -> String { return "not equatable" }
  func a<T: Equatable>(_ val: T) -> String { return "equatable" }
}
class Derived1_52598: Base1_52598 {
  override func a<T>(_ val: T) -> String { return super.a(val) } // okay
  override func a<T: Equatable>(_ val: T) -> String { return super.a(val) } // okay
}

protocol P_52598 {
  associatedtype Bar
}
struct S_52598: P_52598 {
  typealias Bar = Int
}
class Base2_52598 {
  init<F: P_52598>(_ arg: F) where F.Bar == Int {}
}
class Derived2_52598: Base2_52598 {
  init(_ arg1: Int) { super.init(S_52598()) } // okay, doesn't crash
}

// https://github.com/apple/swift/issues/54147

public protocol P_54147 {}
public protocol Q_54147: P_54147 {
    associatedtype A
}

public class Base_54147<F, A> {}

public class Derived_54147<F, A>
  : Base_54147<Base_54147<F, A>, A>,
    Q_54147 {}

public extension Base_54147 where F: Q_54147 {
    static func foo(_: F.A) {}
}
extension Derived_54147 where F: P_54147 {
    public static func foo(_: A) {}
}

// Make sure we don't crash on generic requirement mismatch
protocol P3 {}

protocol P4 {
  associatedtype T
}

class Base {
  func method<T: P3>(_: T) {}
  func method<T: P4>(_: T) where T.T : P3 {}
}

class Derived: Base {
  override func method<T: P3>(_: T) {}
}
