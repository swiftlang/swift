// {"kind":"typecheck","original":"0a6d4af4","signature":"swift::AvailabilityContext::Storage::get(swift::AvailabilityRange const&, bool, llvm::ArrayRef<swift::AvailabilityContext::DomainInfo>, swift::ASTContext const&)"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func append < c : a where b == c>()
  struct d : a {
    typealias b = Int
    func append()
  }
}
