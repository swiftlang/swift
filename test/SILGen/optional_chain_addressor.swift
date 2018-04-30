// RUN: %target-swift-emit-silgen -verify -parse-as-library -enable-sil-ownership %s

func foo(x: UnsafeMutablePointer<UnsafeMutablePointer<()>?>) { // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = x.pointee?.pointee
  _ = x.pointee.map { type(of: $0) }
}
