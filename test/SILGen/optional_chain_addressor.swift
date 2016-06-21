// RUN: %target-swift-frontend -emit-silgen -verify -parse-as-library %s

func foo(x: UnsafeMutablePointer<UnsafeMutablePointer<()>?>) {
  _ = x.pointee?.pointee
  _ = x.pointee?.dynamicType
}
