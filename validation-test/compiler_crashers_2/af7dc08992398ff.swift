// {"signature":"constrainDomainInfos(llvm::SmallVectorImpl<swift::AvailabilityContext::DomainInfo>&, llvm::ArrayRef<swift::AvailabilityContext::DomainInfo>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@available(*, unavailable) @available(_PackageDescription 3) func a {
  @available(_PackageDescription 5) func b
