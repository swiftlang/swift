// {"kind":"typecheck","original":"9b52c7b1","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (isConcrete()), function getConcrete"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Observation
@propertyWrapper struct a<b, c {
  @Observable class d
    @Observable class e: d
      @a<Int, e> var f
