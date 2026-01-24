// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

func slow() {
  let _: [(UInt64) -> (UInt64)] = [
    { $0+1 },
    { ($0*2)+4 },
    { $0*2 },
    { ($0*2)+16 },
    { $0*5 },
    { $0*6 },
  ]
}
