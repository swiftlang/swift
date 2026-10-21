// {"kind":"typecheck","original":"b0513da5","signature":"swift::rewriting::RequirementMachine::getConformancePath(swift::Type, swift::ProtocolDecl*)","signatureNext":"SubstitutionMap::lookupConformance"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  struct b: a
    protocol c {
      associatedtype d
    }
      e<each f: c>(repeat each f.d)
    struct g    ;
    #h
    {      g
