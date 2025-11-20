// {"kind":"typecheck","original":"454af16a","signature":"swift::Mangle::ASTMangler::appendClosureComponents(swift::CanType, unsigned int, bool, swift::DeclContext const*, llvm::ArrayRef<swift::GenericEnvironment*>)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol P {
}
{
  return a
  func a() -> some P
}
