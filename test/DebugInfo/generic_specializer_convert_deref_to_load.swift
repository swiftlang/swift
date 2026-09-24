// RUN: %target-swift-frontend %s -O -emit-sil -g -o /dev/null

// Verify that specializing a generic function with a debug reconstruction block
// that has op_deref (address-only generic type) correctly converts the deref
// to a load when the substituted type is loadable.

@inline(__always)
func process<T>(_ context: T) -> Int {
    return 1
}

struct S<U> {
    var u: U
    func run() -> Int {
        return process(u)
    }
}

let _ = S(u: 0.5).run()
