// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

public func * (lhs: Int, rhs: Character) -> String {
  return String(repeating: String(rhs), count #^COMPLETE^#: lhs)
}
