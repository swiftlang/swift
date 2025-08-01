// {"kind":"typecheck","original":"702452ac","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a: b  protocol b { macro a
