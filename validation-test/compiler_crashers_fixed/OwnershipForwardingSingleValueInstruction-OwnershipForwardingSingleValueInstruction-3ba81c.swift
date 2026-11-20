// {"extraArgs":["-Osize"],"kind":"emit-sil","signature":"swift::OwnershipForwardingSingleValueInstruction::OwnershipForwardingSingleValueInstruction(swift::SILInstructionKind, swift::SILDebugLocation, swift::SILType, swift::ValueOwnershipKind, bool)","signatureAssert":"Assertion failed: ((preservesOwnershipFlag || ownershipKind != OwnershipKind::Guaranteed) && \"Non directly forwarding instructions can not forward guaranteed \" \"ownership\"), function ForwardingInstruction","signatureNext":"UnconditionalCheckedCastInst::create"}
// RUN: %target-swift-frontend -emit-sil -Osize %s -o /dev/null
public protocol a<b> {
  associatedtype b
}
protocol c {
}
open class d<b> {
  var e: c {
    func f() -> c {
      self as! d & c
    }
    return f()
  }
  public func g(core: d<a<Int>>) {
    core.e
  }
}
