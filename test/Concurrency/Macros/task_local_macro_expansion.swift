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

// CHECK: public nonisolated static let $example: TaskLocal<String> = TaskLocal(wrappedValue: "hello")
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

// CHECK: @available(OSX 10.9, *)
// CHECK: private nonisolated static let $example: TaskLocal<AvailableValue?> = TaskLocal(wrappedValue: nil)
//
// CHECK:  {
// CHECK:    get {
// CHECK:      $example.get()
// CHECK:    }
// CHECK:  }

// '@usableFromInline' should be copied to the synthesized '$name' projected property
// so that '@inlinable' code can reference it. https://github.com/swiftlang/swift/issues/90093
@available(SwiftStdlib 5.1, *)
public struct UsableFromInline {
  @TaskLocal
  @usableFromInline
  internal static var usableVar: Int?
}

// CHECK: @usableFromInline
// CHECK: internal nonisolated static let $usableVar: TaskLocal<Int?>

@available(SwiftStdlib 5.1, *)
extension UsableFromInline {
  @inlinable
  public static func doWork(with newValue: Int, operation: () -> Void) {
    UsableFromInline.$usableVar.withValue(newValue, operation: operation)
  }
}

struct SPIExample {
  @TaskLocal
  @_spi(ExampleSPI)
  public static var spiVar: Int?
}

// CHECK: @_spi(ExampleSPI)
// CHECK: public nonisolated static let $spiVar: TaskLocal<Int?>

struct SPIAvailableExample {
  @TaskLocal
  @_spi_available(macOS 10.15, *)
  public static var spiAvailableVar: Int?
}

// CHECK: @_spi_available(macOS 10.15, *)
// CHECK: public nonisolated static let $spiAvailableVar: TaskLocal<Int?>
