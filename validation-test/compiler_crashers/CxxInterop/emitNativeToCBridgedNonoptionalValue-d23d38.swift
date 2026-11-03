// {"kind":"emit-silgen","signature":"emitNativeToCBridgedNonoptionalValue(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::CanType, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (bridgedType->isEqual(SGF.getASTContext().getAnyObjectType())), function emitNativeToCBridgedNonoptionalValue","signatureNext":"emitNativeToCBridgedValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen -cxx-interoperability-mode=default -I %S/Inputs/emitNativeToCBridgedNonoptionalValue-985d69 %s

import ConstRefCallback

let cb: @convention(c) (S) -> Void = { _ in }
takesCallback(cb)
