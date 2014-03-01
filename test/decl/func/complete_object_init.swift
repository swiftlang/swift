// RUN: %swift -parse %s -verify


class X {
  init withInt(i: Int) -> Self {
    self.init(withDouble: Double(i))
  }

  init withFloat(f: Float) -> Self { // expected-error{{complete object initializer for 'X' must delegate (with 'self.init')}}
  }

  init withDouble(d: Double) { 
  }
}
