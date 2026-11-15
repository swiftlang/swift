// {"kind":"emit-sil","signature":"(anonymous namespace)::LowerTupleAddrConstructorTransform::run()","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"SILPassManager::runPassOnFunction"}
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null
struct a {
  var b: (Bool, Void)
}
