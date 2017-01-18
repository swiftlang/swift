// RUN: %target-swift-frontend -emit-silgen -verify -parse-as-library %s

func foo(x: UnsafeMutablePointer<UnsafeMutablePointer<()>?>) { // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = x.pointee?.pointee
  _ = x.pointee.map { type(of: $0) }
}
