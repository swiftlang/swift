// RUN: %target-typecheck-verify-swift

// Verifies fix for Github Issue #85837
// https://github.com/swiftlang/swift/issues/85837
//
// 

@resultBuilder public enum TupleBuilder {
    public static func buildPartialBlock<T>(first: T) -> (T) {
        return first
    }

    public static func buildPartialBlock<each A, B>(accumulated: (repeat each A), next: B) -> (repeat each A, B) {
        return (repeat each accumulated, next)
    }
}

func builder<each A>(@TupleBuilder content: ()->(repeat each A)) -> (repeat each A) {
    return content()
}

// Ensure this optimally packs parameters
let built: (String, Int, String) = builder {
        "a"
        2
        "c"
    }
