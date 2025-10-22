// RUN: %target-typecheck-verify-swift

func fn() {
  _ = 1.description
  _ = 1.5.description

  print(1.description)
  print(1.5.description)
}
