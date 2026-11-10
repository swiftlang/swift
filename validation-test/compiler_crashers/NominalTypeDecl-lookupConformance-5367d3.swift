// {"kind":"typecheck","signature":"swift::NominalTypeDecl::lookupConformance(swift::ProtocolDecl*, llvm::SmallVectorImpl<swift::ProtocolConformance*>&) const","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"Self-conformances are only found by the higher-level \" \"swift::lookupConformance() entry point\"), function lookupConformance","signatureNext":"ValueDecl::canInferObjCFromRequirement"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objc protocol a{a} extension a {
  a class b : a
