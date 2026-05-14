// {"kind":"typecheck","original":"c9044817","signature":"swift::ProtocolConformanceRef::getConditionalRequirements() const","signatureNext":"Requirement::checkRequirement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b where a : Equatable
  extension a : Equatable where a<a> : Equatable
