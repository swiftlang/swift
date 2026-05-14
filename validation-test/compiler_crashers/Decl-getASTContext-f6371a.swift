// {"kind":"typecheck","original":"24b7d12a","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (isConcrete()), function getConcrete"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Observation
@attached(extension conformances: a) macro b
@Observable class c
  @Observable class e: c
    @freestanding(declaration arbitrary) macro d <f>(f)
    #d(e
