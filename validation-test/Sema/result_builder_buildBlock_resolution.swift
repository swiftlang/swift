// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -I %t

// This test verifies that `buildBlock` is type-checked together with enclosing context,
// which means that it's not captured into separate variable but rather used directly and
// contextual information can impact overload resolution.

protocol ActionIdentifier: Hashable {
}

struct ActionLookup<Identifier: ActionIdentifier> {
  init(_: Identifier...) {}
}

@resultBuilder
enum ActionLookupBuilder<Identifier: ActionIdentifier> { // expected-note 3{{'Identifier' previously declared here}}
  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> ActionLookup<Identifier> { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in Swift 6}}
    fatalError()
  }

  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> [ActionLookup<Identifier>] { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in Swift 6}}
    []
  }

  static func buildExpression(_ expression: ActionLookup<Identifier>) -> [ActionLookup<Identifier>] {
    []
  }

  static func buildOptional<Identifier: ActionIdentifier>(_ component: [ActionLookup<Identifier>]?) -> [ActionLookup<Identifier>] { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in Swift 6}}
    []
  }
}

enum ActionType: String, ActionIdentifier, CaseIterable {
    case download
    case upload

    public typealias ActionTypeLookup = ActionLookup<Self>
    public typealias ActionTypeLookupBuilder = ActionLookupBuilder<Self>

    @ActionTypeLookupBuilder
    static var test: ActionTypeLookup {
        ActionTypeLookup(
            .download
        )
        if true { // If condition is needed to make sure that `buildOptional` affects `buildBlock` resolution.
            ActionTypeLookup(
                .upload
            )
        }
    }
}
