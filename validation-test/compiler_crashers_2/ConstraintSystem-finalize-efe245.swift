// {"kind":"typecheck","original":"eece2094","signature":"swift::constraints::ConstraintSystem::finalize()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var
    b : () -> some Any = {
      b()
      c
  }
}
