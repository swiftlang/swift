// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Foo {
  enum Inner {
    case bar
  }
}

let prop1 = 1

if prop1 is Foo {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
if prop1 is Foo.Inner {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
// CHECK: [[@LINE-2]]:17 | enum/Swift | Inner |

let prop2: Int? = 1

if prop2 is Foo {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
if prop2 is Foo.Inner {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
// CHECK: [[@LINE-2]]:17 | enum/Swift | Inner |

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
