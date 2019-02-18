// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Foo {
  enum Inner {
    case bar
  }
}

let a: Foo.Inner = .bar

if case .bar = a {}
// CHECK: [[@LINE-1]]:10 | {{.*}} | bar
if case Foo.Inner.bar = a {}
// CHECK: [[@LINE-1]]:19 | {{.*}} | bar
// CHECK: [[@LINE-2]]:9 | {{.*}} | Foo
// CHECK: [[@LINE-3]]:13 | {{.*}} | Inner
switch a {
case .bar:
// CHECK: [[@LINE-1]]:7 | {{.*}} | bar
    break
case Foo.Inner.bar:
// CHECK: [[@LINE-1]]:16 | {{.*}} | bar
// CHECK: [[@LINE-2]]:6 | {{.*}} | Foo
// CHECK: [[@LINE-3]]:10 | {{.*}} | Inner
    break
default:
    break
}
