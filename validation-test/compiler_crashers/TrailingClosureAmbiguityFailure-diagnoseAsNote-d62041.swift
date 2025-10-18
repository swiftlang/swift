// {"kind":"typecheck","signature":"swift::constraints::TrailingClosureAmbiguityFailure::diagnoseAsNote()","signatureAssert":"Assertion failed: (!empty()), function back"}
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
