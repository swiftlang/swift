// {"signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  let a = $0.c ;
  switch a {
    case let \ (b) where b:
  }
}
