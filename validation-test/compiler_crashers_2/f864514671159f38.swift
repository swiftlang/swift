// {"signature":"swift::constraints::MissingContextualConformanceFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
protocol a }
                       {
                      b -> a? in return .c<d e
