// Constant globals should "work" even when used across files in non-WMO builds.
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: rdar146405994
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -primary-file %s/file1.swift -parse-as-library -enable-experimental-feature CompileTimeValues

//--- file1.swift

@const let constGlobal1: Int = 42

//--- file2.swift

@const let constGlobal2: Int = constGlobal1
