// RUN: %target-typecheck-verify-swift -parse-as-library

#if FAIL
class C {}

func > (lhs: C, rhs: C) -> Bool {
  return false
}
#endif
