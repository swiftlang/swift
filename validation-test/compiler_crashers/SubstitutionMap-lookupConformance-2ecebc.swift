// {"kind":"typecheck","original":"b0513da5","signature":"swift::SubstitutionMap::lookupConformance(swift::CanType, swift::ProtocolDecl*) const","signatureNext":"LookUpConformanceInSubstitutionMap"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b: a
    protocol c {
      associatedtype d
    }
      e<each f: c>(repeat each f.d)
    struct g    ;
    #h
    {      g
