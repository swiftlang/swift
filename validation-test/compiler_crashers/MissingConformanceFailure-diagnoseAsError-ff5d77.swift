// {"kind":"typecheck","original":"abf6d355","signature":"swift::constraints::MissingConformanceFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"MissingConformance::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b: AnyObject> {
}
\a<a>.c
