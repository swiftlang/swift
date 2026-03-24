// {"kind":"typecheck","original":"83528f1a","signature":"swift::ConformanceLookupTable::getConformance(swift::NominalTypeDecl*, swift::ConformanceLookupTable::ConformanceEntry*)","signatureAssert":"Assertion failed: (isConcrete()), function getConcrete","signatureNext":"ConformanceLookupTable::lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Observation
@Observable class a {
  @Observable class b: a {
    init()
  }
}
