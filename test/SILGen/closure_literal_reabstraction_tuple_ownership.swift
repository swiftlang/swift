// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -verify %s

// rdar://83186202

struct Butt {
    var foo: [Int: (Int, String, (Error?) -> Void)]

    func bar() {
        _ = foo.lazy.compactMap { $0.value.0 }
    }
}
