// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=2000

func f896() { let _ = ((0 >> ((0 >> 0) + ((0 / 0) & 0))) >> (0 << ((0 << 0) >> (0 << (0 << 0))))) }
