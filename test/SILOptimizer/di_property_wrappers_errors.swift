// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -emit-sil -verify %s -enable-ownership-stripping-after-serialization

@propertyWrapper
final class ClassWrapper<T> {
  var wrappedValue: T {
    didSet {
      print("  .. set \(wrappedValue)")
    }
  }

  init(wrappedValue initialValue: T) {
    print("  .. init \(initialValue)")
    self.wrappedValue = initialValue
  }

  deinit {
    print("  .. deinit \(wrappedValue)")
  }
}

struct IntStructWithClassWrapper {
  @ClassWrapper var wrapped: Int

  init() {
    wrapped = 42 // expected-error{{'self' used before all stored properties are initialized}}
    // expected-note@-1{{'self.wrapped' not initialized}}
  } // expected-error{{return from initializer without initializing all stored properties}}
  // expected-note@-1{{'self.wrapped' not initialized}}

  init(conditional b: Bool) {
     if b {
       self._wrapped = ClassWrapper(wrappedValue: 32)
     } else {
       wrapped = 42 // expected-error{{'self' used before all stored properties are initialized}}
      // expected-note@-1{{'self.wrapped' not initialized}}
     }
  } // expected-error{{return from initializer without initializing all stored properties}}
  // expected-note@-1{{'self.wrapped' not initialized}}

  init(dynamic b: Bool) {
    if b {
      wrapped = 42 // expected-error{{'self' used before all stored properties are initialized}}
      // expected-note@-1{{'self.wrapped' not initialized}}
    }
    wrapped = 27 // expected-error{{'self' used before all stored properties are initialized}}
    // expected-note@-1{{'self.wrapped' not initialized}}
  } // expected-error{{return from initializer without initializing all stored properties}}
  // expected-note@-1{{'self.wrapped' not initialized}}  
}

// SR_11477

@propertyWrapper
struct SR_11477_W {
  let name: String

  init<T: ExpressibleByIntegerLiteral>(_ value: T = 0) {
    self.name = "Init"
  }

  var wrappedValue: Int {
    get { return 0 }
  }
}

struct SR_11477_S {
  @SR_11477_W var foo: Int
  init() {} // expected-error {{return from initializer without initializing all stored properties}} expected-note {{'self.foo' not initialized}}
}
