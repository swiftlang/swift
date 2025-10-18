// RUN: not %target-swift-frontend %s -typecheck

// rdar://problem/23719809&23720006

func ~=() {}
func ~=(_: Int) {}
func ~=(_: () -> ()) {}

switch 0 {
case 0:
    break
}
