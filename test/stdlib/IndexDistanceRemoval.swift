// RUN: %target-typecheck-verify-swift -swift-version 4

struct Int64Distance<Element>: Collection {
  let _storage: [Element]
  
  typealias Index = Int64
  typealias IndexDistance = Int64
  
  var startIndex: Index { return Int64(_storage.startIndex) }
  var endIndex: Index { return Int64(_storage.startIndex) }
  func index(after i: Index) -> Index { return i + 1 }
  
  subscript(i: Index) -> Element { return _storage[Int(i)] }
}

let c = Int64Distance(_storage: [1,2,3])

let i64: Int64 = 2
_ = c.index(c.startIndex, offsetBy: i64) // expected-warning {{'index(_:offsetBy:)' is deprecated: all index distances are now of type Int}}{{documentation-file=deprecated-declaration}}

let _: Int64 = c.distance(from: c.startIndex, to: c.endIndex) // expected-warning {{distance(from:to:)' is deprecated: all index distances are now of type Int}}{{documentation-file=deprecated-declaration}}

