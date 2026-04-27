// {"kind":"typecheck","original":"1893811d","signature":"swift::TypeChecker::coercePatternToType(swift::ContextualPattern, swift::Type, swift::TypeResolutionOptions, llvm::function_ref<std::__1::optional<swift::Pattern*> (swift::Pattern*, swift::Type)>)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create","signatureNext":"ExprWalker::rewriteTarget"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<each b> {
  case c((repeat each b))
  var d: Int {
    switch self {
    case .c:
      0
    }
  }
}
