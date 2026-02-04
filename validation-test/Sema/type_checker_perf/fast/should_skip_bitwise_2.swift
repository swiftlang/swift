// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=4000 -solver-enable-optimize-operator-defaults -solver-enable-prune-disjunctions

func f1a() { let _ = ((1 << 1) + (~(~((1 * 1) - (-(1 + (~(1)))))))) << ((1 - 1) * 1) }
func f1b() { let _: Int8 = ((1 << 1) + (~(~((1 * 1) - (-(1 + (~(1)))))))) << ((1 - 1) * 1) }

func f2a() { let _ = ((0 >> ((0 >> 0) + ((0 / 0) & 0))) >> (0 << ((0 << 0) >> (0 << (0 << 0))))) }
func f2b() { let _: Int8 = ((0 >> ((0 >> 0) + ((0 / 0) & 0))) >> (0 << ((0 << 0) >> (0 << (0 << 0))))) }

func f3a() { let _ = ((~(1 * 1)) >> (1 - 1)) << ((1 - 1) + (-(-(1)))) }
func f3b() { let _: Int8 = ((~(1 * 1)) >> (1 - 1)) << ((1 - 1) + (-(-(1)))) }