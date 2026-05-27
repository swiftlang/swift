// {"kind":"typecheck","original":"345da5df","signature":"swift::Requirement::Requirement(swift::RequirementKind, swift::Type, swift::Type)","signatureAssert":"Assertion failed: (first), function Requirement","signatureNext":"RequirementMachine::buildRequirementsFromRules"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  func
    c<each d, each e>() -> (repeat ((each d), each e)) where repeat b == ((), each e)
  {
    (repeat {
    })
  }
}
