// {"kind":"emit-sil","original":"470ba694","signature":"swift::LinearLifetimeChecker::ErrorBuilder::handleLeak(llvm::function_ref<void ()>&&)","signatureNext":"LinearLifetimeChecker::checkValueImpl"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol a {
}
struct b: a {
}
class c {
  func d(m: Int, e: Int) throws {
  }
  func f(l: [a]) throws -> [Int] {
  }
}
async {
  let g = c()
  let l = [b()]
  async let h = g.f(l: l)
  async let i = g.d(m: 3, e: 1)
  let (j, k) = try! await (h, i)
}
