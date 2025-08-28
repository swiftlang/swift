// {"kind":"complete","signature":"swift::Parser::parseStorageRestrictionsAttribute(swift::SourceLoc, swift::SourceLoc)","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ #^COMPLETE^#@storageRestrictions(
