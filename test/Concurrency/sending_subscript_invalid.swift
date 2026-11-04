// RUN: %target-swift-frontend -swift-version 6 -typecheck -verify -verify-additional-prefix no-coroutine-accessors- %s
// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature CoroutineAccessors -typecheck -verify -verify-additional-prefix coroutine-accessors- -DCOROUTINE_ACCESSORS %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_CoroutineAccessors

class NS: ~Sendable {}

// MARK: - _read/_modify

struct ReadModify {
    var storage = NS()
    subscript(key: String) -> sending NS {
        _read { // expected-no-coroutine-accessors-error {{'sending' result type cannot be used with '_read' accessor}}
                // expected-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding borrow' accessor}}
            yield storage // fails to diagnose yielding a value in non-disconnected region
        }
        _modify { // expected-no-coroutine-accessors-error {{'sending' result type cannot be used with '_modify' accessor}}
                  // expected-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding mutate' accessor}}
            yield &storage // fails to diagnose yielding a value in non-disconnected region
        }
    }
}

// MARK: - yielding borrow/mutate

#if COROUTINE_ACCESSORS
struct YieldingBorrowMutate {
    var storage = NS()
    subscript(key: String) -> sending NS {
        yielding borrow { // expected-no-coroutine-accessors-error {{'yielding borrow' accessor is only valid when experimental feature coroutine accessors is enabled}}
                          // expected-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding borrow' accessor}}
            yield storage // fails to diagnose yielding a value in non-disconnected region
        }
        yielding mutate { // expected-no-coroutine-accessors-error {{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
                          // expected-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding mutate' accessor}}
            yield &storage // fails to diagnose yielding a value in non-disconnected region
        }
    }
}
#else
struct YieldingBorrowMutate {
    var storage = NS()
    subscript(key: String) -> sending NS {
        yielding borrow { // expected-no-coroutine-accessors-error {{'yielding borrow' accessor is only valid when experimental feature coroutine accessors is enabled}}
                          // expected-no-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding borrow' accessor}}
            yield storage // fails to diagnose yielding a value in non-disconnected region
        }
        yielding mutate { // expected-no-coroutine-accessors-error {{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
                          // expected-no-coroutine-accessors-error @-1 {{'sending' result type cannot be used with 'yielding mutate' accessor}}
            yield &storage // fails to diagnose yielding a value in non-disconnected region
        }
    }
}
#endif

// MARK: - borrow/mutate

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

// MARK: - unsafeAddress/unsafeMutableAddress

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

// MARK: - willSet/didSet

struct Observer {
    subscript(key: String) -> sending NS { // expected-error {{subscript declarations must have a getter}}
        willSet { } // expected-error {{'willSet' is not allowed in subscripts}}
        didSet { } // expected-error {{'didSet' is not allowed in subscripts}}
    }
}
