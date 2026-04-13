// {"kind":"typecheck","original":"4cd97732","signature":"CheckRepressions::add(swift::Type, swift::InverseTypeRepr&, llvm::PointerUnion<swift::TypeDecl const*, swift::ExtensionDecl const*>)","signatureAssert":"Assertion failed: (!ty->is<ExistentialMetatypeType>()), function add","signatureNext":"checkInheritanceClause"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: ~a.Type {
}
