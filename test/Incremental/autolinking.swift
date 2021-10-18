// This test shares inputs with its cousin autolinking-overlay.swift
// This one specifically tests the bare minimum: two empty swift files.

// RUN: %empty-directory(%t)
// RUN: cp %s %t
// RUN: cp -r %S/Inputs/autolinking/* %t
// RUN: cd %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(AutolinkingTest)) -autolink-force-load -module-link-name swiftAutolinkingTest -incremental -driver-show-incremental -module-name AutolinkingTest -output-file-map ofm.json autolinking.swift autolinking-other.swift

// Make sure `swift_FORCE_LOAD_$_swiftAutolinkingTest` appears in all objects
// RUN: llvm-readobj -symbols -coff-exports %t/autolinking.o | %FileCheck %s
// RUN: llvm-readobj -symbols -coff-exports %t/autolinking-other.o | %FileCheck %s
// RUN: llvm-readobj -symbols -coff-exports %t/%target-library-name(AutolinkingTest) | %FileCheck %s

// CHECK: _swift_FORCE_LOAD_$_swiftAutolinkingTest
