// {"signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a
  protocol b {
    associatedtype c
    associatedtype d
    init(
      e
      : c -> d
    struct f
    : b {
g: d init(e
        : a -> _
