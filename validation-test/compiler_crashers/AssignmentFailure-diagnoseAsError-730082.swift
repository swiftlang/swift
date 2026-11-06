// {"kind":"typecheck","signature":"swift::constraints::AssignmentFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier","signatureNext":"RValueTreatedAsLValueFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a let _ a.init = 0
