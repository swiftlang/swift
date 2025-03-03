// Constant globals should "work" even in top-level code mode.

// RUN: %target-swift-frontend -emit-ir -primary-file %s
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal: Int = 42
