// RUN: %swift -I %S/Inputs -parse %s -verify

import other_class

extension OtherClass {
  init() {
    self.init(5) // okay
  }

  init(d: Double) { // expected-error{{initializer for 'OtherClass' defined outside module 'other_class' must delegate (with 'self.init')}}
    value = Int(d)
  }
}

extension OtherSubClass {
  init() {
    self.init(5) // okay
  }

  init(d: Double) {
    super.init(Int(d)) // expected-error{{initializer for 'OtherSubClass' defined outside module 'other_class' must delegate (with 'self.init')}}
  }
}
