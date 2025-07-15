// {"kind":"typecheck","original":"5af4a4fb","signature":"swift::constraints::ConstraintSystem::getBindingsFor(swift::TypeVariableType*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  func b(arr: [a]) {
    arr.compactMap {
      { c in
        d ?? {
          $0..
        }
      }
    }
  }
}
