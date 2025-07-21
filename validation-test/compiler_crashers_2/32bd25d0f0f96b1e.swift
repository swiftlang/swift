// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getTypeOfReference(swift::ValueDecl*, swift::FunctionRefInfo, swift::constraints::ConstraintLocatorBuilder, swift::DeclContext*, swift::constraints::PreparedOverloadBuilder*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((__shared b) -> Void)
func c(UnsafeMutablePointer<UInt8>) a {
  c(&$0
