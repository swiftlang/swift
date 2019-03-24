// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s

@propertyDelegate
public struct Wrapper<T> {
  public var value: T
}

public struct HasDelegates {
  public var x: Int by public Wrapper
}

// CHECK: 9:17 | struct/Swift | Int | s:Si | Ref | rel: 0
// CHECK: 9:31 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
