// RUN: %swift -emit-silgen %s | FileCheck %s

class Foo {
  // CHECK-LABEL: sil  @_TFC10extensions3Foo3zimfS0_FT_T_
  func zim() {}
}

extension Foo {
  // CHECK-LABEL: sil  @_TFC10extensions3Foo4zangfS0_FT_T_
  func zang() {}
}

struct Bar {
  // CHECK-LABEL: sil  @_TFV10extensions3Bar4zungfS0_FT_T_
  func zung() {}
}

extension Bar {
  // CHECK-LABEL: sil  @_TFV10extensions3Bar4zoomfS0_FT_T_
  func zoom() {}
}


