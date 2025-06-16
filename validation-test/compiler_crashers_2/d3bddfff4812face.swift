// {"signature":"swift::constraints::SubscriptMisuseFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
"" [.subscript
