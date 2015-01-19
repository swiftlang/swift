// RUN: %target-parse-verify-swift

@override // expected-error {{'override' can only be specified on class members}} expected-error {{'override' is a declaration modifier, not an attribute}}
func virtualAttributeCanNotBeUsedInSource() {}

class MixedKeywordsAndAttributes {
  // expected-error@+1 {{expected declaration}} expected-error@+1 {{consecutive declarations on a line must be separated by ';'}}
  override @objc func f1() {}
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

@objc class ObjCClass {}

class A {
  func f0() { }
  func f1() { } // expected-note{{overridden declaration is here}}

  var v1: Int { return 5 }
  var v2: Int { return 5 } // expected-note{{overridden declaration is here}}
  var v4: String { return "hello" }// expected-note{{attempt to override property here}}
  var v5: A { return self }
  var v6: A { return self }
  var v7: A { // expected-note{{attempt to override property here}}
    get { return self }
    set { }
  }
  var v8: Int = 0  // expected-note {{attempt to override property here}}
  var v9: Int { return 5 } // expected-note{{attempt to override property here}}

  subscript (i: Int) -> String {
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-note{{overridden declaration is here}}
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (i: Int8) -> A {
    get { return self }
  }

  subscript (i: Int16) -> A { // expected-note{{attempt to override subscript here}}
    get { return self }
    set { }
  }

  @objc subscript (a: ObjCClass) -> String { // expected-note{{overridden declaration here has type '(ObjCClass) -> String'}}
    get { return "hello" }
  }

  func overriddenInExtension() {} // expected-note {{overridden declaration is here}}
}

class B : A {
  override func f0() { }
  func f1() { } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}
  override func f2() { } // expected-error{{method does not override any method from its superclass}}

  override var v1: Int { return 5 }
  var v2: Int { return 5 } // expected-error{{overriding declaration requires an 'override' keyword}}
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

  override subscript (i: Int) -> String {
    get {
      return "hello"
    }

    set {
    }
  }

  subscript (d: Double) -> String { // expected-error{{overriding declaration requires an 'override' keyword}}
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

  // Covariant
  override subscript (i: Int8) -> B {
    get { return self }
  }

  override subscript (i: Int16) -> B { // expected-error{{cannot override mutable subscript of type '(Int16) -> B' with covariant type '(Int16) -> A'}}
    get { return self }
    set { }
  }

  // Objective-C
  @objc subscript (a: ObjCClass) -> Int { // expected-error{{overriding keyed subscript with incompatible type '(ObjCClass) -> Int'}}
    get { return 5 }
  }

  override init() { }
  override deinit { } // expected-error{{'override' modifier cannot be applied to this declaration}}
  override typealias Inner = Int // expected-error{{'override' modifier cannot be applied to this declaration}}
}

extension B {
  override func overriddenInExtension() {} // expected-error{{declarations in extensions cannot override yet}}
}

struct S {
  override func f() { } // expected-error{{'override' can only be specified on class members}}
}
extension S {
  override func ef() {} // expected-error{{method does not override any method from its superclass}}
}

enum E {
  override func f() { } // expected-error{{'override' can only be specified on class members}}
}

protocol P {
  override func f() // expected-error{{'override' can only be specified on class members}}
}

override func f() { } // expected-error{{'override' can only be specified on class members}}

// Invalid 'override' on declarations inside closures.
var rdar16654075a = { // expected-error {{unable to infer closure type in the current context}}
  override func foo() {}
}
var rdar16654075b = { // expected-error {{unable to infer closure type in the current context}}
  class A {
    override func foo() {}
  }
}
var rdar16654075c = { () -> () in
  override func foo() {} // expected-error {{'override' can only be specified on class members}}
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
  required override init(double: Double) { } // expected-warning{{'override' is implied when overriding a required initializer}}
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
  init(double: Double) { } // expected-error{{'required' modifier must be present on all overrides of a required initializer}}
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

