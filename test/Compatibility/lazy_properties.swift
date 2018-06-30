// RUN: %target-typecheck-verify-swift -parse-as-library -swift-version 3

class ReferenceSelfInLazyProperty {
  lazy var refs = (i, f())
  // expected-error@-1 {{cannot use instance member 'i' within property initializer; property initializers run before 'self' is available}}
  lazy var trefs: (Int, Int) = (i, f())
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var qrefs = (self.i, self.f())
  lazy var qtrefs: (Int, Int) = (self.i, self.f())

  lazy var crefs = { (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var ctrefs: (Int, Int) = { (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var cqrefs = { (self.i, self.f()) }()
  lazy var cqtrefs: (Int, Int) = { (self.i, self.f()) }()

  lazy var mrefs = { () -> (Int, Int) in return (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var mtrefs: (Int, Int) = { return (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var mqrefs = { () -> (Int, Int) in (self.i, self.f()) }()
  lazy var mqtrefs: (Int, Int) = { return (self.i, self.f()) }()

  lazy var lcqrefs = { [unowned self] in (self.i, self.f()) }()
  lazy var lcqtrefs: (Int, Int) = { [unowned self] in (self.i, self.f()) }()

  lazy var lmrefs = { [unowned self] () -> (Int, Int) in return (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}
  lazy var lmtrefs: (Int, Int) = { [unowned self] in return (i, f()) }()
  // expected-error@-1 {{instance member 'i' cannot be used on type 'ReferenceSelfInLazyProperty'}}

  lazy var lmqrefs = { [unowned self] () -> (Int, Int) in (self.i, self.f()) }()
  lazy var lmqtrefs: (Int, Int) = { [unowned self] in return (self.i, self.f()) }()

  var i = 42
  func f() -> Int { return 0 }
}

class ReferenceStaticInLazyProperty {
  lazy var refs1 = i
  lazy var refs2 = f()
  // expected-error@-1 {{use of unresolved identifier 'f'}}

  lazy var trefs1: Int = i
  lazy var trefs2: Int = f()
  // expected-error@-1 {{use of unresolved identifier 'f'}}

  static var i = 42
  static func f() -> Int { return 0 }
  // expected-note@-1 {{did you mean 'f'?}}
}
