// {"kind":"emit-silgen","original":"359c5c82","signature":"swift::SILFunction::verify(swift::CalleeCache*, swift::DominanceInfo*, bool, bool, bool) const","signatureAssert":"Assertion failed: (type->isTypeParameter() && \"Expected a type parameter\"), function requiresProtocol","signatureNext":"Lowering::SILGenModule::postEmitFunction"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
actor a<c> {
  var b: c {
    let d: (Error) -> Void = { [unowned self] e in
      b
    }
  }
}
