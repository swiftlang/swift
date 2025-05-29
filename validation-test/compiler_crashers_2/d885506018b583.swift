// {"signature":"swift::AvailabilityContext::Storage::get(swift::AvailabilityRange const&, bool, llvm::ArrayRef<swift::AvailabilityContext::DomainInfo>, swift::ASTContext const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a extension a where b == Self{c : } typealias b : Sequence
