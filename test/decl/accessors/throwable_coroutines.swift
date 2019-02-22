// RUN: %target-typecheck-verify-swift

extension String: Error {}

/// Coroutine overrides ///

class CoroutineC1 {
  var a: Int {
    _read {} // expected-note {{overridden declaration is here}}
  }

  var b: Int {
    get {}
    _modify {} // expected-note {{overridden declaration is here}}
  }
}

class CoroutineOverrideC1: CoroutineC1 {
  override var a: Int {
    _read throws {} // expected-error {{cannot override non-throwing method with throwing method}}
  }

  override var b: Int {
    get {}
    _modify throws {} // expected-error {{cannot override non-throwing method with throwing method}}
  }
}

protocol CoroutineP1 {
  var a: Int { get set } // expected-note {{requirement 'a' declared here}}
}

struct CoroutineS1: CoroutineP1 { // expected-error {{type 'CoroutineS1' does not conform to protocol 'CoroutineP1'}}
  var a: Int { // expected-error {{cannot satisfy the requirement of a non-throwing accessor with a throwing one}}
    _read {}
    _modify throws {}
  }
}

/// Throwing coroutines ///

struct ThrowingCoroutine {
  var _storage: Int = 0

  var a: Int {
    _read throws {
      throw "Coroutine read error" // Okay
    }
    _modify throws {
      throw "Coroutine modify error" // Okay
    }
  }

  var b: Int {
    _read throws {
      throw "Coroutine read error" // Okay
    }

    _modify {
      yield &_storage // Okay
    }
  }
}