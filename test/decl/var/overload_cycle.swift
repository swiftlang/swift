// RUN: %target-typecheck-verify-swift

// Validating the pattern binding initializer for `raw` causes recursive
// validation of the VarDecl. Overload resolution relies on getting back an
// ErrorType from VarDecl validation to disqualify the recursive candidate.
public struct Cyclic {
  static func pickMe(please: Bool) -> Int { return 42 }
  public static let pickMe = Cyclic.pickMe(please: true)
}
