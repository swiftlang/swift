// RUN: %target-swift-emit-silgen -verify -parse-as-library %s

func foo(x: UnsafeMutablePointer<UnsafeMutablePointer<()>?>) { // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = x.pointee?.pointee
  _ = x.pointee.map { type(of: $0) }
}
REQUIRES: updating_for_owned_noescape
