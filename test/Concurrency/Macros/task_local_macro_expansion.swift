// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -typecheck -verify -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s -dump-macro-expansions 2>&1 | %FileCheck %s

struct Nein {
  @TaskLocal
  public static var example: String = "hello"
}

// CHECK: public static let $example: TaskLocal<String> = TaskLocal(wrappedValue: "hello")
//
// CHECK:  {
// CHECK:    get {
// CHECK:      $example.get()
// CHECK:    }
// CHECK:  }

struct Available {
  @available(OSX 10.9, *)
  struct AvailableValue {}

  @TaskLocal
  @available(OSX 10.9, *)
  private static var example: AvailableValue?
}

// CHECK: private static let $example: TaskLocal<AvailableValue?> = TaskLocal(wrappedValue: nil)
//
// CHECK:  {
// CHECK:    get {
// CHECK:      $example.get()
// CHECK:    }
// CHECK:  }
