// RUN: %target-typecheck-verify-swift

func foo(optional: Int?) {
  _ = { [value = optional ?? 0]
    in
    _ = value
  }

  _ = [1].map { [number = optional ?? 1] value -> String in
    return number.description
  }
}
