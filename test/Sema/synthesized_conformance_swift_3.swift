// RUN: %target-typecheck-verify-swift -swift-version 3
// All of these cases should work in Swift 4, so do a normal without -verify should succeed:
// RUN: %target-swift-frontend -typecheck %s -swift-version 4

struct Struct {}

extension Struct: Equatable {}
// expected-error@-1{{implementation of 'Equatable' cannot be automatically synthesized in an extension in Swift 3}}
extension Struct: Codable {}
// expected-error@-1{{implementation of 'Encodable' cannot be automatically synthesized in an extension in Swift 3}}
// expected-error@-2{{implementation of 'Decodable' cannot be automatically synthesized in an extension in Swift 3}}

final class Final {}
extension Final: Codable {}
// expected-error@-1{{implementation of 'Encodable' cannot be automatically synthesized in an extension in Swift 3}}
// expected-error@-2{{implementation of 'Decodable' cannot be automatically synthesized in an extension in Swift 3}}

class Nonfinal {}
extension Nonfinal: Encodable {}
// expected-error@-1{{implementation of 'Encodable' cannot be automatically synthesized in an extension in Swift 3}}


enum NoValues {
    case a, b
}

// This case has been able to be synthesized since at least Swift 3, so it
// should work in that mode.
extension NoValues: Equatable {}
extension NoValues: Hashable {}

extension NoValues: CaseIterable {}
// expected-error@-1{{implementation of 'CaseIterable' cannot be automatically synthesized in an extension in Swift 3}}
// expected-error@-2{{type 'NoValues' does not conform to protocol 'CaseIterable'}}

enum Values {
    case a(Int), b
}

extension Values: Equatable {}
// expected-error@-1{{implementation of 'Equatable' cannot be automatically synthesized in an extension in Swift 3}}
extension Values: Hashable {}
// expected-error@-1{{implementation of 'Hashable' cannot be automatically synthesized in an extension in Swift 3}}
