// RUN: %target-typecheck-verify-swift

// Used to crash with: apply expression is not marked as throwing or
// non-throwing
struct SR5427 : Error {}
func sr5427(op: (() throws -> Void)?) rethrows { try op?() }
try? sr5427(op: { throw SR5427() })
