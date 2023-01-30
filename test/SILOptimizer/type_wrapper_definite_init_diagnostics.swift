// RUN: %target-swift-frontend -enable-experimental-feature TypeWrappers -enable-copy-propagation=requested-passes-only -emit-sil -primary-file %s -o /dev/null -verify

// REQUIRES: asserts

@typeWrapper
struct Wrapper<W, S> {
  var underlying: S

  init(for: W.Type, storage: S) { self.underlying = storage }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>,
               storageKeyPath path: KeyPath<S, V>) -> V {
    get { underlying[keyPath: path] }
  }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>,
               storageKeyPath path: WritableKeyPath<S, V>) -> V {
    get { underlying[keyPath: path] }
    set { underlying[keyPath: path] = newValue }
  }
}

do {
  @Wrapper
  struct ImmutableTest1 {
    let x: Int

    init(x: Int) {
      self.x = x
      self.x = 0 // expected-error {{immutable value 'self.x' may only be initialized once}}
    }
  }

  @Wrapper
  class ImmutableTest2 {
    let a: Int
    let b: String

    init(a: Int, b: String) {
      self.a = 0
      self.a = a // expected-error {{immutable value 'self.a' may only be initialized once}}

      self.b = b
      self.b = "" // expected-error {{immutable value 'self.b' may only be initialized once}}
    }
  }

  // FIXME: Diagnostics should mention `self.b` or `self.a` and not `self.$storage`

  @Wrapper
  struct ImmutableTest3 {
    let a: Int
    let b: String

    init() {
      self.b = "" // Ok
      self.a = 42 // Ok
    }

    init(a: Int = 42) {
      self.a = a
      print(self.a) // expected-error {{'self' used before all stored properties are initialized}} expected-note {{'self.$storage' not initialized}}
      self.b = ""
    }

    /* FIXME: This test is crashing in findFullInitializationPoints
    init(b: String) {
      self.b = b
      print(self.a)
    }
    */

    init(otherB: String?) {
      self.a = 0

      if let b = otherB {
        self.b = b
      }
    } // expected-error {{return from initializer without initializing all stored properties}} expected-note {{'self.$storage' not initialized}}

    init(optB: String? = nil) {
      if let optB {
        self.b = optB
        print(self.b) // expected-error {{'self' used before all stored properties are initialized}} expected-note {{'self.$storage' not initialized}}
      } else {
        self.b = ""
        print(self.b) // expected-error {{'self' used before all stored properties are initialized}} expected-note {{'self.$storage' not initialized}}
      }

      self.a = 0
    }
  }

  @Wrapper
  struct ImmutableReassignmentTest {
    let x: Int = 42

    init(x: Int) {
      self.x = x // expected-error {{immutable value 'self.x' may only be initialized once}}
    }

    init(optX: Int?) {
      if let x = optX {
        self.x = x // expected-error {{immutable value 'self.x' may only be initialized once}}
      }
    }
  }
}
