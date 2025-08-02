// {"kind":"complete","signature":"swift::Parser::parseStorageRestrictionsAttribute(swift::SourceLoc, swift::SourceLoc)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ #^COMPLETE^#@storageRestrictions(
