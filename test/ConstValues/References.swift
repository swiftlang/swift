// Constant globals referencing other constant globals in their initializer expressions

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let a: Int = 42
_const let b: Int = a
_const let c: Int = b
_const let d: Int = -a
