// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/51599

func test(_ a: [Int], _ f: ((Int) -> Bool)?) {
  _ = a.filter(f!)
}
