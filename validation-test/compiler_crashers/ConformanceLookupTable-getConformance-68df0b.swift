// {"kind":"typecheck","original":"585c0ac4","signature":"swift::ConformanceLookupTable::getConformance(swift::NominalTypeDecl*, swift::ConformanceLookupTable::ConformanceEntry*)","signatureAssert":"Assertion failed: (isConcrete()), function getConcrete"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Observation
@Observable class a {
  class b: a {
  }
}
