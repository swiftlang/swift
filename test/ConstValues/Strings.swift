// Constant globals on strings

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: String = "hello"
_const let constGlobal2: StaticString = "hello"
_const let constGlobal3: Int = "hello".count
