// {"kind":"typecheck","signature":"swift::constraints::MissingOptionalUnwrapFailure::offerForceUnwrapFixIt(swift::Expr const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b: Int?{ int: Int { b
