// RUN: %swift -parse %s -verify

struct ValidImmutable {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return base
    }
  }
}

struct ValidBoth {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return UnsafePointer(base)
    }
    mutableAddress {
      return base
    }
  }
}

struct OnlyMutable {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    mutableAddress { // expected-error {{subscript must provide 'address' if it provides 'mutableAddress'}}
      return base
    }
  }
}

struct Repeated {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    address { // expected-note {{previous definition}}
      return UnsafePointer(base)
    }
    address { // expected-error {{duplicate definition}}
      return base // expected-error {{'UnsafeMutablePointer<Int>' is not convertible to 'UnsafePointer<Int>'}}
    }
  }
}

struct RepeatedMutable {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return UnsafePointer(base)
    }
    mutableAddress { // expected-note {{previous definition}}
      return base
    }
    mutableAddress { // expected-error {{duplicate definition}}
      return base
    }
  }
}

struct AddressorAndGet {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return UnsafePointer(base)
    }
    get { // expected-error {{subscript cannot provide both an addressor and a getter/setter}}
      return base.get()
    }
  }
}

struct AddressorAndSet {
  var base: UnsafePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return UnsafePointer(base)
    }
    set { // expected-error {{subscript cannot provide both an addressor and a getter/setter}}
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
    address { return nil }
  }
}

struct ObedientImmutableAddressor: HasImmutableSubscript {
  subscript(index: Int) -> Int {
    address { return nil }
  }
}

struct ObedientMutableAddressor: HasMutableSubscript {
  subscript(index: Int) -> Int {
    address { return nil }
    mutableAddress { return nil }
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
    address { return nil }
  }
}

struct ObedientImmutableAddressor2: HasMutatingImmutableSubscript {
  subscript(index: Int) -> Int {
    address { return nil }
  }
}

struct ObedientMutableAddressor2: HasMutatingMutableSubscript {
  subscript(index: Int) -> Int {
    address { return nil }
    mutableAddress { return nil }
  }
}

// Non-mutating accessor requirements cannot be implemented by mutating accessors.

protocol HasNonMutatingMutableSubscript {
  subscript(index: Int) -> Int { get nonmutating set } // expected-note {{protocol requires}}
}

struct DisobedientNonMutatingMutableAddressor: HasNonMutatingMutableSubscript { // expected-error {{does not conform}}
  subscript(index: Int) -> Int {
    address { return nil }
    mutableAddress { return nil } // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
  }
}

struct ObedientNonMutatingMutableAddressor: HasNonMutatingMutableSubscript {
  subscript(index: Int) -> Int {
    address { return nil }
    nonmutating mutableAddress { return nil }
  }
}
