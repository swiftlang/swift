// {"kind":"typecheck","signature":"swift::constraints::MissingContextualConformanceFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (Context != CTP_Unused), function diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a }
                       {
                      b -> a? in return .c<d e
