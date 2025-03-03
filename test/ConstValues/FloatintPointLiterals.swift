// Constant globals on simple floating-point literals

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1 = 42.0
_const let constGlobal2: Double = 42.0
_const let constGlobal3: Float = 42.0
_const let constGlobal4: Float16 = 42.0
