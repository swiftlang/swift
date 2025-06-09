// {"signature":"swift::constraints::MissingContextualConformanceFailure::diagnoseAsError()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a }
                       {
                      b -> a? in return .c<d e
