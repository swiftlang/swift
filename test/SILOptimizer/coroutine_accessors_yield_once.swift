// RUN: %target-swift-frontend -sil-verify-all -emit-sil -primary-file %s -o /dev/null -verify \
// RUN:     -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_CoroutineAccessors

// Tests for yield-once diagnostics emitted for the `yielding borrow` and
// `yielding mutate` coroutine accessors, which are yield-once-2 coroutines.

struct TestNoYield {
  var stored: Int

  var computed: Int {
    yielding borrow {
    } // expected-error {{accessor must yield before returning}}

    yielding mutate {
    } // expected-error {{accessor must yield before returning}}
  }
}

extension Never {
  var computed: Never? {
    yielding borrow {
      Optional<Never>.none
    } // expected-error {{accessor must yield before returning}}
  }
}

struct TestReturnPathWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating yielding borrow {
      if flag {   // expected-note {{missing yield when the condition is false}}
        yield stored
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}

    yielding mutate {
      if flag {   // expected-note {{missing yield when the condition is false}}
        yield &stored
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestMultipleYields {
  var stored: Int

  var computed: Int {
    yielding borrow {
      yield stored  // expected-note {{previous yield was here}}
      yield stored  // expected-error {{accessor must not yield more than once}}
    }

    yielding mutate {
      yield &stored // expected-note {{previous yield was here}}
      yield &stored // expected-error {{accessor must not yield more than once}}
    }
  }
}

struct TestYieldInLoop {
  var stored: Int

  var computed: Int {
    yielding borrow {
      for _ in 0 ..< 10 {
        yield stored  // expected-note {{previous yield was here}}
                      // expected-error@-1 {{accessor must not yield more than once}}
      }
    }
  }
}

struct TestYieldInOptionalBinding<T> {
  var storedOpt: T?

  var computed: T {
    yielding borrow {
      if let stored = storedOpt {
        yield stored
      }                   // expected-note {{missing yield in the nil case}}
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestValidYields {
  var stored: Int
  var flag: Bool

  var computed: Int {
    yielding borrow {
      yield stored
    }

    yielding mutate {
      yield &stored
    }
  }

  var branching: Int {
    mutating yielding borrow {
      if flag {
        yield stored
      } else {
        yield 0
      }
    }
  }
}
