// RUN: %target-swift-frontend -enable-experimental-feature LifetimeDependence -emit-sil -verify %s

// REQUIRES: swift_feature_LifetimeDependence

@lifetime(copy x)
func id<T: ~Escapable>(_ x: T) -> T {
    return x
}

func double<T>(_ f: @escaping (T) -> T) -> (T) -> T {
    return { f(f($0)) }
}

func doubleid() -> (Int) -> Int {
    // Substituting `id` with `T = Int` renders its return type Escapable,
    // eliminating the lifetime dependency and allowing the function to be
    // used as a `(T) -> T` value.
    return double(id)
}

func doubleidGeneric<U>(_: U.Type) -> (U) -> U {
    // Substituting `id` with `T = U` renders its return type Escapable,
    // eliminating the lifetime dependency and allowing the function to be
    // used as a `(T) -> T` value.
    return double(id)
}

@lifetime(x: copy y)
func overwrite<T: ~Escapable>(_ x: inout T, _ y: T) {
    x = y
}

func doubleInout<T>(_ f: @escaping (inout T, T) -> Void) -> (inout T, T) -> Void {
    return {
        f(&$0, $1)
        f(&$0, $1)
    }
}

func doubleOverwrite() -> (inout Int, Int) -> Void {
    // Substituting `id` with `T = Int` renders the `inout` parameter Escapable,
    // eliminating the lifetime dependency and allowing the function to be
    // used as a `(inout T, T) -> T` value.
    return doubleInout(overwrite)
}

func doubleOverwriteGeneric<U>(_: U.Type) -> (inout U, U) -> Void {
    // Substituting `id` with `T = U` renders the `inout` parameter Escapable,
    // eliminating the lifetime dependency and allowing the function to be
    // used as a `(inout T, T) -> T` value.
    return doubleInout(overwrite)
}
