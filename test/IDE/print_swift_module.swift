// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/print_swift_module.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_swift_module.swiftdoc %s
// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=print_swift_module -I %t -source-filename=%s > %t.syn.txt
// RUN: %target-swift-ide-test -print-module -access-filter-internal -no-empty-line-between-members -module-to-print=print_swift_module -I %t -source-filename=%s > %t.syn.internal.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK2 < %t.syn.internal.txt

public protocol P1 {
  /// foo1 comment from P1
  func foo1()
  /// foo2 comment from P1
  func foo2()
}
public class C1 : P1 {
  public func foo1() {
  }
  /// foo2 comment from C1
  public func foo2() {
  }
}

/// Alias comment
public typealias Alias<T> = (T, T)

/// returnsAlias() comment
public func returnsAlias() -> Alias<Int> {
  return (0, 0)
}

@resultBuilder
public struct BridgeBuilder {
    public static func buildBlock(_: Any...) {}
}

public struct City {
  public init(@BridgeBuilder builder: () -> ()) {}
}

// CHECK1:      /// Alias comment
// CHECK1-NEXT: typealias Alias<T> = (T, T)

// CHECK1:      public class C1 : print_swift_module.P1 {
// CHECK1-NEXT:   /// foo1 comment from P1
// CHECK1-NEXT:   public func foo1()
// CHECK1-NEXT:   /// foo2 comment from C1
// CHECK1-NEXT:   public func foo2()
// CHECK1-NEXT: }

// CHECK1: public init(@print_swift_module.BridgeBuilder builder: () -> ())

// CHECK1:      public protocol P1 {
// CHECK1-NEXT:   /// foo1 comment from P1
// CHECK1-NEXT:   func foo1()
// CHECK1-NEXT:   /// foo2 comment from P1
// CHECK1-NEXT:   func foo2()
// CHECK1-NEXT: }

// CHECK1:      /// returnsAlias() comment
// CHECK1-NEXT: func returnsAlias() -> print_swift_module.Alias<Int>

// CHECK2: struct Event {
public struct Event {
  public var start: Int
  public var end: Int?
  public var repeating: ((), Int?)
  public var name = "untitled event"

  // CHECK2: init(start: Int, end: Int? = nil, repeating: ((), Int?) = ((), nil), name: String = "untitled event")
}
// CHECK2: }
