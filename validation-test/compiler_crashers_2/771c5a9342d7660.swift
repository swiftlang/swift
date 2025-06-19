// {"signature":"swift::BoundGenericType::getExpandedGenericArgs()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a func b [{ lazy var c = a(d var e = b
