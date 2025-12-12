// {"kind":"typecheck","original":"b10f3bb6","signature":"swift::Mangle::ASTMangler::appendAnyGenericType(swift::GenericTypeDecl const*, swift::Mangle::ASTMangler::BaseEntitySignature&)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  extension a where Self == b? {
    enum b
      let c: some a
