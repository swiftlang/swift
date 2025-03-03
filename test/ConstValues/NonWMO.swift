// Constant globals should "work" even when used across files in non-WMO builds.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -primary-file %s/file1.swift -parse-as-library

//--- file1.swift

_const let constGlobal1: Int = 42

//--- file2.swift

_const let constGlobal2: Int = constGlobal1
