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
