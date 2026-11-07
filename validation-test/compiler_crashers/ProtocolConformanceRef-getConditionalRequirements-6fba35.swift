// {"kind":"typecheck","original":"c9044817","signature":"swift::ProtocolConformanceRef::getConditionalRequirements() const","signatureNext":"Requirement::checkRequirement"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b where a : Equatable
  extension a : Equatable where a<a> : Equatable
