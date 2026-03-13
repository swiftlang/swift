// {"kind":"emit-silgen","original":"7b4a72a9","signature":"swift::Lowering::SILGenBuilder::createEnum(swift::SILLocation, swift::Lowering::ManagedValue, swift::EnumElementDecl*, swift::SILType)","signatureAssert":"Assertion failed: (v->getType().isObject()), function operator()"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
var a: Any = ["": [a]] as [String: [AnyObject?]]
