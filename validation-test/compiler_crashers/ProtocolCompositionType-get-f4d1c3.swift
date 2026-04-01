// {"kind":"typecheck","original":"cd65bd41","signature":"swift::ProtocolCompositionType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>, swift::InvertibleProtocolSet, bool)","signatureNext":"TypeResolver::resolveCompositionType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b, c> {
  associatedtype b
  associatedtype c
}
protocol d: a {
  protocol e: d {
    typealias f = a & a<String, String> & e
  }
}
