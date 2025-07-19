// {"signature":"swift::TypeChecker::typeCheckCheckedCast(swift::Type, swift::Type, swift::CheckedCastContextKind, swift::DeclContext*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
init(a : () -> ()) {
  [.init ?? a]
}
