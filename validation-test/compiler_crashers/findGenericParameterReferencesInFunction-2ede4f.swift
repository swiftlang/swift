// {"extraArgs":["-enable-experimental-feature","BuiltinModule","-language-mode","6"],"kind":"typecheck","signature":"findGenericParameterReferencesInFunction(swift::CanGenericSignature, swift::GenericTypeParamType*, swift::GenericTypeParamType*, swift::AnyFunctionType const*, swift::TypePosition, bool, std::__1::optional<unsigned int>)","signatureNext":"findGenericParameterReferences"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature BuiltinModule -language-mode 6 %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
Builtin.makeBorrow(0 as Any)
