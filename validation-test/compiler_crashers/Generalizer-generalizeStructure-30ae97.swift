// {"kind":"emit-ir","original":"68961feb","signature":"(anonymous namespace)::Generalizer::generalizeStructure(swift::CanType)","signatureAssert":"Assertion failed: (!origType->hasRepresentation()), function visitExistentialMetatypeType","signatureNext":"ExistentialTypeGeneralization::get"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a<b> {
  associatedtype b
}
struct c {
}
@available(SwiftStdlib 5.7, *)
func d(
  e: [a<c>.Type]
) async {
  await withTaskGroup { f in
    for g in e {
      f.addTask {
        g
      }
    }
  }
}
