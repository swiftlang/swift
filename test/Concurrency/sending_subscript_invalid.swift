// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature CoroutineAccessors -parse -verify %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_CoroutineAccessors

class NS: ~Sendable {}

// MARK: - _read + set with sending (incompatible)

struct ReadModify {
    var storage = NS()
    subscript(key: String) -> sending NS {
        _read { // expected-error {{'sending' result type cannot be used with '_read' accessor}}
            yield storage // fails to diagnose yielding a value in non-disconnected region
        }
      _modify { // expected-error {{'sending' result type cannot be used with '_modify' accessor}}
          yield &storage // fails to diagnose yielding a value in non-disconnected region
      }
    }
}

// MARK: - yielding borrow + yielding mutate with sending (incompatible)

struct YieldingBorrowMutate {
    var storage = NS()
    subscript(key: String) -> sending NS {
        yielding borrow { // expected-error {{'sending' result type cannot be used with 'yielding borrow' accessor}}
            yield storage // fails to diagnose yielding a value in non-disconnected region
        }
        yielding mutate { // expected-error {{'sending' result type cannot be used with 'yielding mutate' accessor}}
            yield &storage // fails to diagnose yielding a value in non-disconnected region
        }
    }
}

// MARK: - borrow + mutate with sending (incompatible)

struct BorrowMutate {
    var storage = NS()
    subscript(key: String) -> sending NS {
        borrow { // expected-error {{'sending' result type cannot be used with 'borrow' accessor}}
            return storage
        }
        mutate { // expected-error {{'sending' result type cannot be used with 'mutate' accessor}}
            return &storage
        }
    }
}

// MARK: - unsafeAddress with sending (incompatible)

struct Address {
    var storage: UnsafeMutablePointer<NS>
    subscript(key: String) -> sending NS {
        unsafeAddress { // expected-error {{'sending' result type cannot be used with 'unsafeAddress' accessor}}
            UnsafePointer(storage)
        }
        unsafeMutableAddress { // expected-error {{'sending' result type cannot be used with 'unsafeMutableAddress' accessor}}
            storage
        }
    }
}

// MARK: - willSet/didSet on subscript with sending (parser error, no crash)

struct Observer {
    subscript(key: String) -> sending NS { // expected-error {{subscript declarations must have a getter}}
        willSet { } // expected-error {{'willSet' is not allowed in subscripts}}
        didSet { } // expected-error {{'didSet' is not allowed in subscripts}}
    }
}
