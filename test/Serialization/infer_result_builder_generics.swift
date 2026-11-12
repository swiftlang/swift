// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name InferResultBuilderGenerics -o %t %S/Inputs/infer_result_builder_generics_other.swift
// RUN: %target-swift-ide-test -print-module -module-to-print=InferResultBuilderGenerics -I %t -source-filename=%s | %FileCheck %s --check-prefix=PRINT
// RUN: %target-swift-frontend -typecheck -I %t %s

// Check that result builder attributes with omitted generic arguments or
// placeholder types are serialized, and that clients infer the generic
// arguments when applying the result builder transform.

// PRINT-LABEL: struct Invocation {
// PRINT:   @ArrayBuilder var arguments: () -> [String]
// PRINT:   init(@ArrayBuilder arguments: @escaping () -> [String])
// PRINT: }
// PRINT: func makeArray(@ArrayBuilder _ build: () -> [String]) -> [String]
// PRINT: func makeDictionary(@DictionaryBuilder<String, <<placeholder>>> _ build: () -> [String : Int]) -> [String : Int]
// PRINT: func makeNestedArray(@Array<<<placeholder>>>.Builder _ build: () -> [Int]) -> [Int]

import InferResultBuilderGenerics

let array: [String] = makeArray {
  "a"
  "b"
}

let dictionary: [String: Int] = makeDictionary {
  ("a", 1)
  ("b", 2)
}

let nestedArray: [Int] = makeNestedArray {
  1
  2
}

let invocation = Invocation {
  "format"
  "--in-place"
}
