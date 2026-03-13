// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend -typecheck -parse-as-library -external-plugin-path %swift-plugin-dir#%swift-plugin-server -primary-file %s %S/Inputs/ObservableClass2.swift

// RUN: %target-swift-frontend -typecheck -parse-as-library -external-plugin-path %swift-plugin-dir#%swift-plugin-server %s -primary-file %S/Inputs/ObservableClass2.swift

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.9, *)
let x = AnotherObservableClass(name: "Hello")
