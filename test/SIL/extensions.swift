// RUN: %swift -emit-sil %s | FileCheck %s

class Foo {
  // CHECK: sil @zim
  func zim() {}
}

extension Foo {
  // CHECK: sil @zang
  func zang() {}
}

struct Bar {
  // CHECK: sil @zung
  func zung() {}
}

extension Bar {
  // CHECK: sil @zoom
  func zoom() {}
}


