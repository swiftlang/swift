// {"kind":"complete","original":"4a1f764f","signature":"swift::objc_translation::getNameForObjC(swift::ValueDecl const*, swift::objc_translation::CustomNamesOnly_t)","signatureAssert":"Assertion failed: (isa<ClassDecl>(VD) || isa<ProtocolDecl>(VD) || isa<StructDecl>(VD) || isa<EnumDecl>(VD) || isa<EnumElementDecl>(VD) || isa<TypeAliasDecl>(VD)), function getNameForObjC","signatureNext":"getObjCNameForSwiftDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: objc_interop
@objc protocol a {
  macro b() = #^^#
}
