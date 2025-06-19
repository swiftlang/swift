// {"signature":"swift::constraints::AllowTypeOrInstanceMemberFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b<c >(c = a [
       extension a {
subscript->Int
