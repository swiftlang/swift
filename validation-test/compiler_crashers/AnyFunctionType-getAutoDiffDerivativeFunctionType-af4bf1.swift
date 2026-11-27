// {"kind":"emit-silgen","signature":"swift::AnyFunctionType::getAutoDiffDerivativeFunctionType(swift::IndexSubset*, swift::AutoDiffDerivativeFunctionKind, llvm::function_ref<swift::ProtocolConformanceRef (swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)>, swift::GenericSignature, bool)","signatureAssert":"Assertion failed: (linearMapTypeExpected && \"Linear map type is invalid\"), function getAutoDiffDerivativeFunctionType","signatureNext":"Lowering::TypeConverter::makeConstantInterfaceType"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -enable-upcoming-feature InternalImportsByDefault -module-name Library -o %t %t/Library.swift
// RUN: not --crash %target-swift-frontend -emit-silgen -I %t %t/main.swift
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
// REQUIRES: swift_feature_InternalImportsByDefault
//--- Library.swift
import _Differentiation
@differentiable(reverse)
public func foo(_ x: Float) -> Float {
  x
}
//--- main.swift
@_weakLinked import Library
if #_hasSymbol(foo) {
}
