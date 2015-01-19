// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
struct Foo {
  // Allocating constructor - should have no line table info.
  // CHECK: _TFV12Constructors3FooCfMS0_FT1xSi_S0_{{.*}}[ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] [scope 0] [init]
  init(x: Int) {}
  func bar(x: Int) {}
}

var f = Foo(x: 1)
f.bar(2)
