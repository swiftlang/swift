// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple

// expected-warning@+1 {{use of protocol 'Collection' as a type must be written 'any Collection'; this will be an error in a future Swift language mode}}
func test(c: Collection<Int>) -> Int {
  c.count
}
