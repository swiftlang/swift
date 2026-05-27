// {"kind":"typecheck","original":"7b234c4f","signature":"performEndOfPipelineActions(swift::CompilerInstance&)","signatureAssert":"Assertion failed: (ctx.Diags.hadAnyError() || !ctx.hasDelayedConformanceErrors() && \"Encountered invalid conformance without emitting error?\"), function performEndOfPipelineActions","signatureNext":"performCompile"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
actor c {
  func d<e: a>(f: e) -> e.b {
  }
}
func m(m: c) {
  func g(h: Int) {
  }
  withoutActuallyEscaping(g) { i in
    struct j: a {
      init(h: Int, k: (Int) -> Void)
    }
    let l = j(h: 0, k: i)
    Task {
      m.d(f: l)
    }
  }
}
