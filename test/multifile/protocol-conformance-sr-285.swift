// RUN: not --crash %target-swift-frontend -emit-ir -o - -primary-file %s %S/Inputs/sr-285-other.swift -module-name main

// SR-285: Crash in IR generation due to missing conformance.
func definedInMain() { print(MemoryLayout<FourFloats>.size) }

let d = definedInOther()
