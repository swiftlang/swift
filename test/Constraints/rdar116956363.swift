// RUN: %target-typecheck-verify-swift

// rdar://116956363 - Make sure we don't crash here.
@resultBuilder
struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
}

func bar(_ fn: () -> Void) -> Int { 0 }

@Builder
func foo(_ b: Bool) -> Int {
  bar {
    let _ = if b {
      1
    } else {
      2
    }
  }
}
