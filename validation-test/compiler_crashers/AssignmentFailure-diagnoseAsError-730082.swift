// {"kind":"typecheck","signature":"swift::constraints::AssignmentFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (!baseName.isSpecial() && \"Can't retrieve the identifier of a special base name\"), function getBaseIdentifier","signatureNext":"RValueTreatedAsLValueFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a let _ a.init = 0
