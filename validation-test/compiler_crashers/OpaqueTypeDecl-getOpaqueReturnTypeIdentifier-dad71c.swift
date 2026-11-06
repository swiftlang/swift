// {"kind":"typecheck","original":"2e672fab","signature":"swift::OpaqueTypeDecl::getOpaqueReturnTypeIdentifier() const","signatureNext":"SourceFile::getOpaqueReturnTypeDecls"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  protocol c {
    struct d {
      protocol e
        struct f<g
          func h -> f<some e>
