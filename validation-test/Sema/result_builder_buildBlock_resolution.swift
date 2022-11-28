// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -I %t -enable-experimental-feature ResultBuilderASTTransform

// This test verifies that `buildBlock` is type-checked together with enclosing context,
// which means that it's not captured into separate variable but rather used directly and
// contextual information can impact overload resolution.

protocol ActionIdentifier: Hashable {
}

struct ActionLookup<Identifier: ActionIdentifier> {
  init(_: Identifier...) {}
}

@resultBuilder
enum ActionLookupBuilder<Identifier: ActionIdentifier> {
  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> ActionLookup<Identifier> {
    fatalError()
  }

  static func buildBlock<Identifier: ActionIdentifier>(_ components: [ActionLookup<Identifier>]...) -> [ActionLookup<Identifier>] {
    []
  }

  static func buildExpression(_ expression: ActionLookup<Identifier>) -> [ActionLookup<Identifier>] {
    []
  }

  static func buildOptional<Identifier: ActionIdentifier>(_ component: [ActionLookup<Identifier>]?) -> [ActionLookup<Identifier>] {
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
