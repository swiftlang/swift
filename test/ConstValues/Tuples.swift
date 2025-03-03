// Constant globals on tuples

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int = 42
_const let constGlobal2: (Int, Int) = (42, 42)
_const let constGlobal3: (Int, Bool) = (42, true)
_const let constGlobal4: (Int, (Int, Int)) = (42, (42, 42))
_const let constGlobal5: (Int, Float) = (42, 42.0)
_const let constGlobal6: (Int, String) = (42, "Foo")
_const let constGlobal7: (UInt64, StaticString, @convention(c) ()->Int) = (42, "hi", { return 42 })
