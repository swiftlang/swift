// RUN: %batch-code-completion

// Make sure we don't crash.
func foo(xs: [[Int]], ys: [Int]) {
    for x in ys {
      _ = xs.map{ $0.filter{ $0 == x } #^COMPLETE^# }
      // COMPLETE: Begin completions
    }
}
