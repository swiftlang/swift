// RUN: %swift -parse-stdlib -parse %s -verify

import Swift

struct ValidImmutable {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress {
      return base
    }
  }
}

struct ValidBoth {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress {
      return UnsafePointer(base)
    }
    unsafeMutableAddress {
      return base
    }
  }
}

struct OnlyMutable {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeMutableAddress { // expected-error {{subscript must provide either a getter or 'address' if it provides 'mutableAddress'}}
      return base
    }
  }
}

struct Repeated {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress { // expected-note {{previous definition}}
      return UnsafePointer(base)
    }
    unsafeAddress { // expected-error {{duplicate definition}}
      return base // expected-error {{'UnsafeMutablePointer<Int>' is not convertible to 'UnsafePointer<Int>'}}
    }
  }
}

struct RepeatedMutable {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress {
      return UnsafePointer(base)
    }
    unsafeMutableAddress { // expected-note {{previous definition}}
      return base
    }
    unsafeMutableAddress { // expected-error {{duplicate definition}}
      return base
    }
  }
}

struct AddressorAndGet {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress { // expected-error {{subscript cannot provide both 'address' and a getter}}
      return UnsafePointer(base)
    }
    get {
      return base.get()
    }
  }
}

struct AddressorAndSet {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress {
      return UnsafePointer(base)
    }
    set { // expected-error {{subscript cannot provide both 'address' and a setter; use an ordinary getter instead}}
    }
  }
}

struct MutableAddressorAndGet {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeMutableAddress {
      return base
    }
    get {
      return base.memory
    }
  }
}

protocol HasImmutableSubscript {
  subscript(index: Int) -> Int { get }
}
protocol HasMutableSubscript {
  subscript(index: Int) -> Int { get set } // expected-note {{protocol requires}}
}

struct DisobedientImmutableAddressor: HasMutableSubscript { // expected-error {{does not conform}}
  subscript(index: Int) -> Int { // expected-note {{candidate is not settable}}
    unsafeAddress { return nil }
  }
}

struct ObedientImmutableAddressor: HasImmutableSubscript {
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
  }
}

struct ObedientMutableAddressor: HasMutableSubscript {
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}

protocol HasMutatingImmutableSubscript {
  subscript(index: Int) -> Int { mutating get }
}
protocol HasMutatingMutableSubscript {
  subscript(index: Int) -> Int { mutating get set } // expected-note {{protocol requires}}
}

// We allow mutating accessor requirements to be implemented by non-mutating accessors.

struct DisobedientImmutableAddressor2: HasMutatingMutableSubscript { // expected-error {{does not conform}}
  subscript(index: Int) -> Int { // expected-note {{candidate is not settable}}
    unsafeAddress { return nil }
  }
}

struct ObedientImmutableAddressor2: HasMutatingImmutableSubscript {
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
  }
}

struct ObedientMutableAddressor2: HasMutatingMutableSubscript {
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}

// Non-mutating accessor requirements cannot be implemented by mutating accessors.

protocol HasNonMutatingMutableSubscript {
  subscript(index: Int) -> Int { get nonmutating set } // expected-note {{protocol requires}}
}

struct DisobedientNonMutatingMutableAddressor: HasNonMutatingMutableSubscript { // expected-error {{does not conform}}
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil } // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
  }
}

struct ObedientNonMutatingMutableAddressor: HasNonMutatingMutableSubscript {
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    nonmutating unsafeMutableAddress { return nil }
  }
}

// FIXME: Actually plumb the work to fix the grammar in these
// diagnostics if/when we productize them.  ("a addressor")
struct RedundantAddressors1 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    unsafeAddress { return nil } // expected-note {{previous definition of addressor is here}}
    addressWithNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a addressor}}
  }
}
struct RedundantAddressors2 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    unsafeAddress { return nil } // expected-note {{previous definition of addressor is here}}
    addressWithPinnedNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a addressor}}
  }
}
struct RedundantAddressors3 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    addressWithNativeOwner { return nil } // expected-note {{previous definition of addressor is here}}
    addressWithPinnedNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a addressor}}
  }
}

struct RedundantMutableAddressors1 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil } // expected-note {{previous definition of mutable addressor is here}}
    mutableAddressWithNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a mutable addressor}}
  }
}
struct RedundantMutableAddressors2 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil } // expected-note {{previous definition of mutable addressor is here}}
    mutableAddressWithNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a mutable addressor}}
  }
}
struct RedundantMutableAddressors3 {
  var owner : Builtin.NativeObject
  subscript(index: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil } // expected-note {{previous definition of mutable addressor is here}}
    mutableAddressWithNativeOwner { return (nil, owner)  } // expected-error {{subscript already has a mutable addressor}}
  }
}
