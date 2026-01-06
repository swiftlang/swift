// RUN: %target-typecheck-verify-swift

struct IdentifiableValue: Identifiable {
  let id = 42
}

class IdentifiableClass: Identifiable {}

extension IdentifiableValue {
  var nextID: ID {
    return id + 1
  }
}
