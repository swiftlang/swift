// RUN: %target-swift-frontend -emit-ir -o - -primary-file %s %S/Inputs/issue-42907-other.swift -module-name main

// https://github.com/apple/swift/issues/42907
// Crash in IR generation due to missing conformance
func definedInMain() { print(MemoryLayout<FourFloats>.size) }

let d = definedInOther()
