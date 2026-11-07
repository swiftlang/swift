// {"kind":"typecheck","original":"16240db9","signature":"swift::DeclContext::getParentModule() const","signatureNext":"ParsedDeclAttrFilter"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@a({
  unowned protocol b
  }
  let c
