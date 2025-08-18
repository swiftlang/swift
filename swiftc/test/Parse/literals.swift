// RUN: %target-typecheck-verify-swift

// Test literal parsing

// Integer literals
let decimal = 123
let binary = 0b1010
let octal = 0o755
let hex = 0xFF

// Floating point literals
let float1 = 3.14
let float2 = 1.23e-4
let float3 = 0x1.8p3

// String literals
let string1 = "Hello"
let string2 = "World"
let escaped = "Line 1\nLine 2"
let quote = "He said \"Hello\""

// Boolean literals
let trueValue = true
let falseValue = false

// Nil literal
let nilValue: Int? = nil

// Array literals
let numbers = [1, 2, 3, 4, 5]
let strings = ["a", "b", "c"]

// Dictionary literals
let dict = ["key1": "value1", "key2": "value2"]