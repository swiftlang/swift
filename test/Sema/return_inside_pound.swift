// RUN: %target-typecheck-verify-swift

func f1() {
  #if DEBUG
    return // no-error
  #endif
}

func f2() {
  #if DEBUG
    return // no-error
  #else
    return // no-error
  #endif
}
