// Constant globals on inline arrays

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -disable-availability-checking

_const let constGlobal1: InlineArray = [1, 2, 3]
_const let constGlobal2: InlineArray = [1.0, 2.0, 3.0]
_const let constGlobal3: InlineArray = constGlobal1
_const let constGlobal4: Int = ([1, 2, 3] as InlineArray).count
