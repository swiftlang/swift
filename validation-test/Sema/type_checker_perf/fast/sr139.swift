// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

// SR-139:
// Infinite recursion parsing bitwise operators
let x = UInt32(0x1FF)&0xFF << 24 | UInt32(0x1FF)&0xFF << 16 | UInt32(0x1FF)&0xFF << 8 | (UInt32(0x1FF)&0xFF)

