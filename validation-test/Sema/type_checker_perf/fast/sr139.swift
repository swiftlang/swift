// RUN: %target-typecheck-verify-swift
// REQUIRES: tools-release,no_asserts

// SR-139:
// Infinite recursion parsing bitwise operators
let x = UInt32(0x1FF)&0xFF << 24 | UInt32(0x1FF)&0xFF << 16 | UInt32(0x1FF)&0xFF << 8 | (UInt32(0x1FF)&0xFF)

