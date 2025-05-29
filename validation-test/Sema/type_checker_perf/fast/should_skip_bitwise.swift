// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=80000

// REQUIRES: tools-release,no_asan

func f2() { let _ = 0 << (~(~(~(~(~(~(~(~(~(~(~(0 << 0)))))))))))) }
