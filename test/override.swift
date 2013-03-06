// RUN: %swift -parse %s -verify -parse-as-library

class A { 
  func f1() { } // expected-note{{overridden declaration is here}}
  func f2() -> A { } // expected-note{{overridden declaration is here}}

  func [objc] f3() { }
  func [objc] f4() -> A { }
}

extension A {
  func f5() { } // expected-note{{overridden declaration is here}}
  func f6() -> A { } // expected-note{{overridden declaration is here}}

  func [objc] f7() { }
  func [objc] f8() -> A { }
}

class B : A { }

extension B { 
  func f1() { }  // expected-error{{declarations in extensions cannot override yet}}
  func f2() -> B { } // expected-error{{declarations in extensions cannot override yet}}

  func f3() { }
  func f4() -> B { }

  func f5() { }  // expected-error{{declarations from extensions cannot be overridden yet}}
  func f6() -> A { }  // expected-error{{declarations from extensions cannot be overridden yet}}

  func [objc] f7() { }
  func [objc] f8() -> A { }
}

func callOverridden(b : B) {
  b.f3()
  b.f4()
  b.f7()
  b.f8()
}
