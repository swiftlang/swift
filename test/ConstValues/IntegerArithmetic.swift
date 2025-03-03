// Constant globals on integer arithmetics

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int = 42
_const let constGlobal2: UInt = 42 + 42
_const let constGlobal3: UInt = 42 * 42
_const let constGlobal4: UInt = 42 - 42
_const let constGlobal5: UInt = 42 / 42
_const let constGlobal6: UInt = 42 % 2
_const let constGlobal7: UInt = (42 % 2)