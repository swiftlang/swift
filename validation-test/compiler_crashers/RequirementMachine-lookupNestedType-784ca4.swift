// {"kind":"typecheck","original":"a43d8500","signature":"swift::rewriting::RequirementMachine::lookupNestedType(swift::Type, swift::Identifier) const","signatureNext":"TypeResolver::resolveDeclRefTypeReprRec"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c
    typealias d = c where d == b.e
