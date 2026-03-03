// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/47148

func add(_ a: Int, _ b: Int) throws -> Int {
    return a + b
}

func add(_ a: Int, _ b: Int) -> Float {
    return Float(a + b)
}

func useAdd() {
    guard let c: Float = try? add(3, 4) else {
        return
    }
    print(c)
}
