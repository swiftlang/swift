// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Foo {
  struct Bar {}
}

let prop1 = 1

if prop1 is Foo {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
if prop1 is Foo.Bar {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
// CHECK: [[@LINE-2]]:17 | struct/Swift | Bar |

let prop2: Int? = 1

if prop2 is Foo {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
if prop2 is Foo.Bar {}
// CHECK: [[@LINE-1]]:13 | struct/Swift | Foo |
// CHECK: [[@LINE-2]]:17 | struct/Swift | Bar |
