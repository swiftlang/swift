// {"kind":"typecheck","signature":"swift::TypeAliasType::TypeAliasType(swift::TypeAliasDecl*, swift::Type, llvm::ArrayRef<swift::Type>, swift::Type, swift::RecursiveTypeProperties)","signatureAssert":"Assertion failed: (isValidParent(parent)), function TypeAliasType","signatureNext":"TypeAliasType::get"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  typealias b = Int
}
struct c<d
  typealias e<d: a> = d.b
  )
  let : e<c> =
