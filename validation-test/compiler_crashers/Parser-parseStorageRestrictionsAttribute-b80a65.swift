// {"kind":"typecheck","signature":"swift::Parser::parseStorageRestrictionsAttribute(swift::SourceLoc, swift::SourceLoc)","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange","signatureNext":"Parser::parseNewDeclAttribute"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@storageRestrictions(
