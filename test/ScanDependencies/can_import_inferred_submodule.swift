// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -F %S/Inputs/Frameworks
// RUN: %validate-json %t/deps.json | %FileCheck %s

#if canImport(WithInferredClangModule.InferredClangModule)
import ScannerTestKit
#endif

// Ensure that the 'WithInferredClangModule.InferredClangModule' can be imported
// CHECK: "swift": "ScannerTestKit"
