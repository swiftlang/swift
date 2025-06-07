// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -I %t

// ! - Based on the result builders proposal `buildBlock` shouldn't be type-checked together with `buildOptional`.

protocol ActionIdentifier: Hashable {
}

struct ActionLookup<Identifier: ActionIdentifier> {
  init(_: Identifier...) {}
}

@resultBuilder
enum ActionLookupBuilder<Identifier: ActionIdentifier> { // expected-note 3{{'Identifier' previously declared here}}
  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> ActionLookup<Identifier> { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}} expected-note {{found this candidate}}
    fatalError()
  }

  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> [ActionLookup<Identifier>] { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}} expected-note {{found this candidate}}
    []
  }

  static func buildExpression(_ expression: ActionLookup<Identifier>) -> [ActionLookup<Identifier>] {
    []
  }

  static func buildOptional<Identifier: ActionIdentifier>(_ component: [ActionLookup<Identifier>]?) -> [ActionLookup<Identifier>] { // expected-warning {{generic parameter 'Identifier' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
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
        if true { // If condition without else is needed to make sure that `buildOptional` affects `buildBlock` resolution.
          // expected-error@-1 {{ambiguous use of 'buildBlock'}}
            ActionTypeLookup(
                .upload
            )
        }
    }
}
