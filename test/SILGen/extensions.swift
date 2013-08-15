// RUN: %swift -emit-silgen %s | FileCheck %s

class Foo {
  // CHECK: sil @_TC10extensions3Foo3zimfS0_FT_T_
  func zim() {}
}

extension Foo {
  // CHECK: sil @_TC10extensions3Foo4zangfS0_FT_T_
  func zang() {}
}

struct Bar {
  // CHECK: sil @_TV10extensions3Bar4zungfRS0_FT_T_
  func zung() {}
}

extension Bar {
  // CHECK: sil @_TV10extensions3Bar4zoomfRS0_FT_T_
  func zoom() {}
}


