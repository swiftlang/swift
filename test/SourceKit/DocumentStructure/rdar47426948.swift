// RUN: %sourcekitd-test -req=structure %s -- %s | %FileCheck %s

class C {
  @IBAction init(foo: Void) {}
  @IBAction init(bar: ()) {}
  @IBAction init(baz: Int) {}
  @IBAction func methodName(foo: ()) {}
  @IBAction func methodName(bar: Void) {}
  @IBAction func methodName(baz: Int) {}
  @IBAction deinit {}
}

// CHECK: {
// CHECK:   key.name: "init(foo:)",
// CHECK-NOT:   key.selector_name
// CHECK: }
// CHECK: {
// CHECK:   key.name: "init(bar:)",
// CHECK-NOT:   key.selector_name
// CHECK: }
// CHECK: {
// CHECK:   key.name: "init(baz:)",
// CHECK-NOT:   key.selector_name
// CHECK: }
// CHECK: {
// CHECK:   key.name: "methodName(foo:)",
// CHECK:   key.selector_name: "methodNameWithFoo:"
// CHECK: }
// CHECK: {
// CHECK:   key.name: "methodName(bar:)",
// CHECK:   key.selector_name: "methodNameWithBar:"
// CHECK: }
// CHECK: {
// CHECK:   key.name: "methodName(baz:)",
// CHECK:   key.selector_name: "methodNameWithBaz:"
// CHECK: }
// CHECK: {
// CHECK:   key.name: "deinit",
// CHECK-NOT:   key.selector_name
// CHECK: }
