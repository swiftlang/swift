// {"kind":"typecheck","original":"e0669768","signature":"swift::ArgumentList::getSourceRange() const","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange","signatureNext":"Dispatch::getSourceRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup class a {
  struct b {
    c: Int
  }
    c,
    func      d
    {
      c
    }
  subscript<e>(dynamicMember f: KeyPath<b, e>)  e
