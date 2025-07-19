// {"kind":"complete","signature":"swift::DeclRefTypeRepr::DeclRefTypeRepr(swift::TypeReprKind, swift::DeclNameRef, swift::DeclNameLoc, unsigned int, bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
{ #^COMPLETE^#@storageRestrictions(
