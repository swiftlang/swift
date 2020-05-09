// RUN: %target-typecheck-verify-swift

enum E1 {
  case a1
  case b1
  case c1
  case d1
  case e1
  case f1
}

enum E2 {
  case a2, b2, c2, d2
}

func foo(s: E1, style: E2) {
  switch (s, style) {
    case (.a1, .a2),
         (.a1, .d2),
         (.c1, .a2),
         (.c1, .d2),
         (.c1, .c2),
         (.a1, .c2):
      break

    case (.a1, .b2),
         (.b1, .b2),
         (.c1, .b2):
        break

    case (.b1, .a2),
         (.b1, .d2),
         (.b1, .c2):
      break

    case (.e1, .a2),
         (.e1, .d2),
         (.e1, .c2):
      break

    case (.e1, .b2):
        break

    case (.d1, .a2),
         (.d1, .d2):

      break

    case (.d1, .b2):
      break

    case (.d1, .c2):
      break

    case (.f1, .a2),
         (.f1, .b2),
         (.f1, .c2),
         (.f1, .d2):
      break
  }
}
