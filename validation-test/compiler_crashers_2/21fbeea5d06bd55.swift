// {"kind":"typecheck","signature":"createEnumSwitch(swift::ASTContext&, swift::DeclContext*, swift::Expr*, swift::EnumDecl*, swift::EnumDecl*, bool, std::__1::function<std::__1::tuple<swift::EnumElementDecl*, swift::BraceStmt*> (swift::EnumElementDecl*, swift::EnumElementDecl*, llvm::ArrayRef<swift::VarDecl*>)>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a : Codable { case x( x : Int b : Double enum XCodingKeys : CodingKey{
x case x case b let c = a
