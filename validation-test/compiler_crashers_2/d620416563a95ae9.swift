// {"signature":"swift::constraints::TrailingClosureAmbiguityFailure::diagnoseAsNote()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  struct a {
    init ();
    init ();
    func callAsFunction< b >(c : b)
  } ;
  a {
  }
}
