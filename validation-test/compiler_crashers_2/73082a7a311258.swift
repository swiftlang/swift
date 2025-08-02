// {"kind":"typecheck","signature":"swift::constraints::AssignmentFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a let _ a.init = 0
