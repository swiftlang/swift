// {"extraArgs":["-enable-experimental-feature","OnceFunctions"],"kind":"emit-silgen","original":"f2ee8d19","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitYieldInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature OnceFunctions %s
// REQUIRES: swift_feature_OnceFunctions
class a<b, c> {
  var e: (b) -> c {
    _read {
    }
    _modify {
    }
  }
}
class c: a<Int, c> {
  override var e: (Int) -> c {
    _read {
    }
    _modify {
      yield &e
    }
  }
}
