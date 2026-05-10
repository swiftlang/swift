// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"31305192","signature":"createEnumSwitch(swift::ASTContext&, swift::DeclContext*, swift::Expr*, swift::EnumDecl*, swift::EnumDecl*, bool, std::__1::function<std::__1::tuple<swift::EnumElementDecl*, swift::BraceStmt*> (swift::EnumElementDecl*, swift::EnumElementDecl*, llvm::ArrayRef<swift::VarDecl*>)>)","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]","signatureNext":"deriveBodyEncodable_enum_encode"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
enum a: b, Codable {
  case c()
}
