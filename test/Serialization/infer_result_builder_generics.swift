// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name InferResultBuilderGenerics -o %t %S/Inputs/infer_result_builder_generics_other.swift
// RUN: %target-swift-ide-test -print-module -module-to-print=InferResultBuilderGenerics -I %t -source-filename=%s | %FileCheck %s --check-prefix=PRINT
// RUN: %target-swift-frontend -typecheck -I %t %s

// Check that the generic arguments inferred for result builder attributes are
// serialized, and that clients can use these declarations.

// PRINT-LABEL: struct Invocation {
// PRINT:   @ArrayBuilder<String> var arguments: () -> [String]
// PRINT:   init(@ArrayBuilder<String> arguments: @escaping () -> [String])
// PRINT: }
// PRINT: func makeArray(@ArrayBuilder<String> _ build: () -> [String]) -> [String]
// PRINT: func makeDictionary(@DictionaryBuilder<String, Int> _ build: () -> [String : Int]) -> [String : Int]
// PRINT: func makeNestedArray(@Array<Int>.Builder _ build: () -> [Int]) -> [Int]
// PRINT: @ArrayBuilder<String> func makeOpaqueArray() -> some Collection<String>
// PRINT: @DictionaryBuilder<String, Int> func makeOpaqueDictionary() -> some Collection<(key: String, value: Int)>

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
