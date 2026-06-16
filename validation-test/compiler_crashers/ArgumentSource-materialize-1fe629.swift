// {"kind":"emit-sil","original":"e70b0376","signature":"swift::Lowering::ArgumentSource::materialize(swift::Lowering::SILGenFunction&, swift::Lowering::AbstractionPattern, swift::SILType) &&","signatureAssert":"Assertion failed: (!destType || destType.getObjectType() == SGF.getLoweredType(origFormalType, substFormalType).getObjectType()), function materialize","signatureNext":"ArgEmitter::emit"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol a {
  associatedtype b
  func c(d: Int) -> b
}
struct e: a {
  func c(d: Int) {
  }
}
struct f {
  subscript(g: StringProtocol) -> some a {
    e()
  }
}
func h() {
  let i = f()
  let j = i[""]
    .c
}
