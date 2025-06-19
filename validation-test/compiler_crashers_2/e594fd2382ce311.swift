// {"signature":"swift::constraints::MissingCallFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b {
  init(repeat each b) func c < each d { a<repeat each d, String >(repeat e, {
    ""
