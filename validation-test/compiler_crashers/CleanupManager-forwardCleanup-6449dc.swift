// {"kind":"emit-silgen","original":"6bdbaea1","signature":"swift::Lowering::CleanupManager::forwardCleanup(swift::DiverseStackBase::stable_iterator)","signatureAssert":"Assertion failed: (cleanup.isActive() && \"forwarding inactive or dead cleanup?\"), function forwardCleanup","signatureNext":"Lowering::ManagedValue::forward"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
}
struct b {
  let c: String
}
enum d: ~Copyable {
  case i((Int, String), a)
}
func f(h: consuming d) {
  switch consume h {
  case .i((0, let g), let e as b) where e.c == g:
    0
  case _:
    0
  }
}
