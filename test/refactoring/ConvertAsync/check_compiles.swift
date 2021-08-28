// REQUIRES: concurrency

// RUN: %empty-directory(%t)

func simple(completion: @escaping () -> Void) { }
func anything() -> Bool { return true }

// RUN: %swift-frontend -typecheck %s
// RUN: not %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6
func cannotCompile() {
  simple {
    if anything() {
      return
    }
  }
}
