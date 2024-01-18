// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -primary-file %s -emit-reference-dependencies-path %t/tuple-members.swiftdeps

func f() {}
func f() -> S {}

struct S {
  var x: Int
}

_ = f().x
