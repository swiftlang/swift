// {"extraArgs":["-sil-verify-all"],"kind":"emit-ir","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"BBState::handleJoinPoint"}
// RUN: not --crash %target-swift-frontend -emit-ir -sil-verify-all %s
protocol a {
  associatedtype b
  func c() -> b
}
@available(SwiftStdlib 6.0, *)
struct d<each e: a> {
  var f: (repeat (each e).b)
  init(g: repeat each e) {
    f = (repeat (each g).c())
  }
}
@available(SwiftStdlib 6.0, *)
func h<each e>(
  j: Int,
  g: repeat each e
) {
  for i in 0..<j {
    if i == 0 {
      switch j {
      case 1:
        while true {
          d()
        }
      default:
        break
      }
    }
  }
}
