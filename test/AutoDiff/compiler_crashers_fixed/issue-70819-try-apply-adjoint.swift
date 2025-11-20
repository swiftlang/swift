// RUN: %target-build-swift %s

// https://github.com/swiftlang/swift/issues/70819
// Fixes "note: do not know how to handle this incoming bb argument" assertion
// Here try_apply is produced so the result value is available in the successor BB

import _Differentiation

@differentiable(reverse)
func test1(input: Float) throws -> Float {
    return input
}

@differentiable(reverse)
func test2(input: Float) -> Float {
    do {
        return try test1(input: input)
    } catch {
        return 0.0
    }
}

let (value, gradient) = valueWithGradient(at: 1.0, of: test2)
print("Value: \(value), gradient: \(gradient)")
