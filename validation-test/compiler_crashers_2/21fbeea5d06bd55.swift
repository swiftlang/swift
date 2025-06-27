// {"signature":"std::__1::__function::__func<deriveBodyDecodable_enum_init(swift::AbstractFunctionDecl*, void*)::$_0, std::__1::allocator<deriveBodyDecodable_enum_init(swift::AbstractFunctionDecl*, void*)::$_0>, std::__1::tuple<swift::EnumElementDecl*, swift::BraceStmt*> (swift::EnumElementDecl*, swift::EnumElementDecl*, llvm::ArrayRef<swift::VarDecl*>)>::operator()(swift::EnumElementDecl*&&, swift::EnumElementDecl*&&, llvm::ArrayRef<swift::VarDecl*>&&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a : Codable { case x( x : Int b : Double enum XCodingKeys : CodingKey{
x case x case b let c = a
