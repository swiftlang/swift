// RUN: %target-typecheck-verify-swift

// Allow inheritance from parenthesized protocol names.
protocol DefaultItem {}

extension DefaultItem {
  var isEnabled: Bool { return true }
}

protocol Item: (DefaultItem) {}

func test(item: Item) {
  _ = item.isEnabled
}

// Don't infer requirements from protocol inheritance clause.
protocol HasAssoc {
  associatedtype T
}

protocol Other {}

typealias Requirements<T> = Other where T : HasAssoc, T.T == Int

protocol Silly : Requirements<Self> { }
// expected-error@-1 {{type 'Self' does not conform to protocol 'HasAssoc'}}
