// Constant globals on integer expressions

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int = (42 + 42 + 42) / 3
_const let constGlobal2: Int = MemoryLayout<UInt32>.size + 4
_const let constGlobal3: Int = Int(17.0 / 3.5)
_const let constGlobal4: Int = constGlobal1 + 1
_const let constGlobal5: Int = -constGlobal1 + 1
