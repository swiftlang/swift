// {"kind":"typecheck","signature":"swift::constraints::SolverTrail::~SolverTrail()","signatureAssert":"Assertion failed: (Changes.empty() && \"Trail corrupted\"), function ~SolverTrail"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a< b { case c(}
         func d< b >(b->a< b >) d(a< e >.c let a
