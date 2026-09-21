// RUN: %target-swift-frontend -module-name test -emit-sil -O %s | %FileCheck %s

public enum MyError<T>: Swift.Error { case fail }

// Check that function signature opt does not crash when remove the dead argument.
public func foo<T>(_ x: T) throws(MyError<T>) -> UInt8 {
  throw .fail
}

// CHECK-LABEL: sil shared @$s4test3fooys5UInt8VxAA7MyErrorOyxGYKlFTf4d_n : $@convention(thin) <T> () -> (UInt8, @error MyError<T>) {
public func run<T>(_ x: T) -> UInt8 {
  do {
    return try foo(x)
  } catch {
    return 0
  }
}
