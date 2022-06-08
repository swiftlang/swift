// RUN: %target-typecheck-verify-swift  -disable-availability-checking -warn-concurrency -parse-as-library
// REQUIRES: concurrency

@MainActor
struct X1: Equatable, Hashable, Codable {
  let x: Int
  let y: String
}

// expected-error@+5{{type 'X2' does not conform to protocol 'Encodable'}}
// expected-error@+4{{type 'X2' does not conform to protocol 'Decodable'}}
// expected-error@+3{{type 'X2' does not conform to protocol 'Equatable'}}
// expected-error@+2{{type 'X2' does not conform to protocol 'Hashable'}}
@MainActor
struct X2: Equatable, Hashable, Codable {
  let x: Int
  var y: String
}

@MainActor
enum X3: Hashable, Comparable, Codable {
  case a
  case b(Int)
}
