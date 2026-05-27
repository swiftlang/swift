// {"kind":"typecheck","original":"8884151d","signature":"swift::TypeChecker::coercePatternToType(swift::ContextualPattern, swift::Type, swift::TypeResolutionOptions, llvm::function_ref<std::__1::optional<swift::Pattern*> (swift::Pattern*, swift::Type)>)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create","signatureNext":"StmtChecker::checkSiblingCaseStmts"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<each b> {
  case c((repeat each b))
  func d() {
    switch self {
    case .c:
    }
  }
}
