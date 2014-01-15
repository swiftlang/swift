// RUN: %swift -parse %s -verify

class OtherClass {
  var value: Int

  init(value: Int) { // expected-error{{initializer for 'OtherClass' defined outside module 'other_class' must delegate (with 'self.init')}}
    self.value = value
  }
}

class OtherSubClass : OtherClass {
  init(value: Int) {
    super.init(value)
  }
}
