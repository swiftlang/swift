// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

enum E1 {
  case X
  case Y
}
enum E2 {
  case A
  case B
  case C
}

func foo() -> ([E1], [E2]) {
  return ([.X], [.A, .B, .#^A^#])
}
