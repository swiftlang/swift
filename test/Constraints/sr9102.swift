// RUN: %target-typecheck-verify-swift

func test(_ a: [Int], _ f: ((Int) -> Bool)?) {
  _ = a.filter(f!)
}
