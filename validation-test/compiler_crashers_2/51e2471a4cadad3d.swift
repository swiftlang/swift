// {"signature":"(anonymous namespace)::ExprRewriter::buildSingleCurryThunk(swift::Expr*, swift::Expr*, swift::DeclContext*, swift::FunctionType*, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation func a(b: AnyClass?) { b (b
