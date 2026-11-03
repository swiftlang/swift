// {"kind":"emit-silgen","signature":"swift::SILParameterInfo const& claimNext<swift::SILParameterInfo>(llvm::ArrayRef<swift::SILParameterInfo>&)","signatureAssert":"Assertion failed: (!array.empty() && \"claiming next from empty array!\"), function claimNext","signatureNext":"TranslateArguments::claimNextInnerParam"}
// RUN: not --crash %target-swift-frontend -emit-silgen -cxx-interoperability-mode=default -I %S/Inputs/claimNext-2650b9 %s

import StaticSelf
protocol P {
  static func make() -> Self
}
extension S: P {}
