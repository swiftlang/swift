// {"kind":"typecheck","original":"3d5176b0","signature":"swift::ActorReferenceResult::Builder::build()","signatureNext":"ActorReferenceResult::forReference"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a
  struct d
    extension Actor where Self: a {
        b<c>(d  -> c)
      let e = a().b { f in
