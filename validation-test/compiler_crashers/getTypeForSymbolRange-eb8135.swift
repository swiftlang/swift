// {"kind":"typecheck","original":"c378d04f","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureNext":"RequirementMachine::buildRequirementsFromRules"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  func
    c<b>()
  {
    extension [b] {
    }
  }
}
