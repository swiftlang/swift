// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

func format(_ string: String) -> String {
  string.prefix(2) + " "
    + string.dropFirst(2).prefix(3) + " "
    + string.dropFirst(5).prefix(3) + "-"
    + string.dropFirst(8).prefix(2) + "-"
    + string.dropFirst(10)
}
