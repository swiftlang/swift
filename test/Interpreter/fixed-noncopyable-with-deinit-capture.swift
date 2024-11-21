// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct EmptyWithDeinit: ~Copyable {
    deinit { print("destroying empty") }
}

struct EmptyWithDeinitGeneric<T>: ~Copyable {
    deinit { print("destroying \(T.self) empty") }
}

struct EmptyWithDeinitGenericIndirect<T>: ~Copyable {
    let value = EmptyWithDeinitGeneric<T>()
}

struct FixedWithDeinit: ~Copyable {
    var field = 0
    deinit { print("destroying fixed") }
}

struct FixedWithDeinitGeneric<T>: ~Copyable {
    var field = 0
    deinit { print("destroying \(T.self) fixed") }
}

struct FixedWithDeinitGenericIndirect<T>: ~Copyable {
    let value = FixedWithDeinitGeneric<T>()
}

@inline(never)
func borrow<T: ~Copyable>(_: borrowing T) {}

@inline(never)
func capture_and_release() -> () -> () {
    let ewd = EmptyWithDeinit()
    return { borrow(ewd) }
}

@inline(never)
func capture_and_release_generic<T>(_: T.Type) -> () -> () {
    let ewd = EmptyWithDeinitGeneric<T>()
    return { borrow(ewd) }
}

@inline(never)
func capture_and_release_generic_indirect<T>(_: T.Type) -> () -> () {
    let ewd = EmptyWithDeinitGeneric<T>()
    return { borrow(ewd) }
}

@inline(never)
func capture_and_release_fixed() -> () -> () {
    let ewd = FixedWithDeinit()
    return { borrow(ewd) }
}

@inline(never)
func capture_and_release_generic_fixed<T>(_: T.Type) -> () -> () {
    let ewd = FixedWithDeinitGeneric<T>()
    return { borrow(ewd) }
}

@inline(never)
func capture_and_release_generic_indirect_fixed<T>(_: T.Type) -> () -> () {
    let ewd = FixedWithDeinitGeneric<T>()
    return { borrow(ewd) }
}

func main() {
    // CHECK: starting
    print("starting")
    do {
        // CHECK-NEXT: destroying empty
        _ = capture_and_release()
    }
    do {
        // CHECK-NEXT: destroying Int empty
        _ = capture_and_release_generic(Int.self)
    }
    do {
        // CHECK-NEXT: destroying String empty
        _ = capture_and_release_generic_indirect(String.self)
    }
    do {
        // CHECK-NEXT: destroying fixed
        _ = capture_and_release_fixed()
    }
    do {
        // CHECK-NEXT: destroying Float fixed
        _ = capture_and_release_generic_fixed(Float.self)
    }
    do {
        // CHECK-NEXT: destroying Double fixed
        _ = capture_and_release_generic_indirect_fixed(Double.self)
    }
}
main()
