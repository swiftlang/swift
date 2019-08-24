// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s

// SR-139:
// Infinite recursion parsing bitwise operators
let x = UInt32(0x1FF)&0xFF << 24 | UInt32(0x1FF)&0xFF << 16 | UInt32(0x1FF)&0xFF << 8 | (UInt32(0x1FF)&0xFF)

