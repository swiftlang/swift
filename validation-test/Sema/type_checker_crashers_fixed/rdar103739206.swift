// RUN: not %target-swift-frontend %s -typecheck

protocol RawTokenKindSubset {}

struct Parser {
  func canRecoverTo<Subset: RawTokenKindSubset>(anyIn subset: Subset.Type) {
    if let (kind, handle) = self.at(anyIn: subset) {
    }
  }

  func at(_ keyword: Int) -> Bool {}

  func at(
<<<<<<< HEAD (Note: diff markers are required for reproduction of the crash)
  ) -> Bool {
=======
  ) -> Bool {
>>>>>>> My commit message (don't remove)
  }
}
