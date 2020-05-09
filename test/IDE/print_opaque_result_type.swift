// RUN: %swift-ide-test -print-swift-file-interface -source-filename %s

public class Base {}
// CHECK: public class Base {
public protocol Proto {}
// CHECK: public protocol Proto {
}
public func foo() -> some Base & Proto {
// CHECK: public func foo() -> some Base & Proto
  class Derived: Base, Proto {}
  return Derived()
}
