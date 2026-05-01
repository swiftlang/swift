// {"kind":"typecheck","original":"3e5b46f8","signature":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b: c, b
  extension a: Codable where b:
