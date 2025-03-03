// Constant globals on optionals

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int? = 42
_const let constGlobal2: Int? = nil
_const let constGlobal3: UInt8? = 42
_const let constGlobal4: UInt8? = nil
_const let constGlobal5: Bool? = true
_const let constGlobal6: Bool? = nil
_const let constGlobal7: (Int, Int)? = (42, 42)
_const let constGlobal8: (Int, Int)? = nil
_const let constGlobal9: String? = "hello"
_const let constGlobal10: String? = nil
