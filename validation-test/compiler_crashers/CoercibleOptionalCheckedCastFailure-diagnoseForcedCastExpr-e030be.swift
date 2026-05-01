// {"kind":"typecheck","original":"8f4e40cf","signature":"swift::constraints::CoercibleOptionalCheckedCastFailure::diagnoseForcedCastExpr() const","signatureNext":"CoercibleOptionalCheckedCastFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let a: Int??.Type = a as! Int.Type?
