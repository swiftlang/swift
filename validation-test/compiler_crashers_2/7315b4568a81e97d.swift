// {"signature":"swift::GenericSignature::verify(llvm::ArrayRef<swift::Requirement>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b where Self : a protocol a
