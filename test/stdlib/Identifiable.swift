// RUN: %target-typecheck-verify-swift

struct IdentifiableValue: Identifiable {
  let id = 42
}

class IdentifiableClass: Identifiable {}