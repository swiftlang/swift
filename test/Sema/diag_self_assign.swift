// RUN: %swift %s -verify

var sa1_global: Int
sa1_global = sa1_global // expected-error {{assigning a variable to itself}}

class SA1 {
  var foo: Int
  init(foo: Int) {
    foo = foo // expected-error {{assigning a variable to itself}}
    self.foo = self.foo // expected-error {{assigning a property to itself}}
    foo = self.foo // no-error
    self.foo = foo // no-error
  }
  def f(foo: Int) {
    foo = foo // expected-error {{assigning a variable to itself}}
    self.foo = self.foo // expected-error {{assigning a property to itself}}
    foo = self.foo // no-error
    self.foo = foo // no-error
  }
}

class SA2 {
  var foo: Int {
  get:
    return 0
  set:
  }
  init(foo: Int) {
    foo = foo // expected-error {{assigning a variable to itself}}
    self.foo = self.foo // expected-error {{assigning a property to itself}}
    foo = self.foo // no-error
    self.foo = foo // no-error
  }
  def f(foo: Int) {
    foo = foo // expected-error {{assigning a variable to itself}}
    self.foo = self.foo // expected-error {{assigning a property to itself}}
    foo = self.foo // no-error
    self.foo = foo // no-error
  }
}

class SA3 {
  var foo: Int {
  get:
    return foo
  set:
    foo = foo // expected-error {{assigning a property to itself}}
    self.foo = self.foo // expected-error {{assigning a property to itself}}
    foo = self.foo // expected-error {{assigning a property to itself}}
    self.foo = foo // expected-error {{assigning a property to itself}}
  }
}

class SA4 {
  var foo: Int {
  get:
    return foo
  set:
    value = value // expected-error {{assigning a variable to itself}}
  }
}

class SA5 {
  var foo: Int
}
def SA5_test(a: SA4, b: SA4) {
  a.foo = a.foo // expected-error {{assigning a property to itself}}
  a.foo = b.foo
}

def SA6_test(a: Int) {
  a = a.0 // expected-error {{assigning a variable to itself}}
  a.0 = a // expected-error {{assigning a variable to itself}}
}
