// {"kind":"typecheck","signature":"constrainDomainInfos(llvm::SmallVectorImpl<swift::AvailabilityContext::DomainInfo>&, llvm::ArrayRef<swift::AvailabilityContext::DomainInfo>)","signatureAssert":"Assertion failed: (!isConstrained), function constrainDomainInfos"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@available(*, unavailable) @available(_PackageDescription 3) func a {
  @available(_PackageDescription 5) func b
