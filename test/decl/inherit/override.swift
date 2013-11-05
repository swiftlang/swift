// RUN: %swift -parse %s -verify -parse-as-library

class A { 
  def f1() { } // expected-note{{overridden declaration is here}}
  def f2() -> A { } // expected-note{{overridden declaration is here}}

  @objc def f3() { }
  @objc def f4() -> A { }
}

extension A {
  def f5() { } // expected-note{{overridden declaration is here}}
  def f6() -> A { } // expected-note{{overridden declaration is here}}

  @objc def f7() { }
  @objc def f8() -> A { }
}

class B : A { }

extension B { 
  def f1() { }  // expected-error{{declarations in extensions cannot override yet}}
  def f2() -> B { } // expected-error{{declarations in extensions cannot override yet}}

  def f3() { }
  def f4() -> B { }

  def f5() { }  // expected-error{{declarations from extensions cannot be overridden yet}}
  def f6() -> A { }  // expected-error{{declarations from extensions cannot be overridden yet}}

  @objc def f7() { }
  @objc def f8() -> A { }
}

def callOverridden(b : B) {
  b.f3()
  b.f4()
  b.f7()
  b.f8()
}
