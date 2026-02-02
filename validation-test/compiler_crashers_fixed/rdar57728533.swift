// RUN: not %target-swift-frontend -typecheck %s

protocol Item {
  associatedtype Rule
  func isValide(valid: Rule) -> Bool
}

protocol PairableItem: Item {
  associatedtype AssociatedItem: PairableItem where AssociatedItem.Rule: Rule
}
