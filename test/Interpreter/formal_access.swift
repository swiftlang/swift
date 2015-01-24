// RUN: %target-run-simple-swift | FileCheck %s

class C: Printable {
  var value: Int
  init(_ v: Int) { value = v }

  var description: String { return String(value) }
}

var global = [C(1), C(2)]

println("Begin")
println("1. global[0] == \(global[0])")
// CHECK:      Begin
// CHECK-NEXT: 1. global[0] == 1

func doit(inout local: C) {
  println("2. local == \(local)")
  println("2. global[0] == \(global[0])")
  // CHECK-NEXT: 2. local == 1
  // CHECK-NEXT: 2. global[0] == 1

  // There's a connection between 'local' and 'global[0]'.
  local = C(4)
  println("3. local == \(local)")
  println("3. global[0] == \(global[0])")
  // CHECK-NEXT: 3. local == 4
  // CHECK-NEXT: 3. global[0] == 4

  // This assignment is to a different index and so is
  // not allowed to cause unspecified behavior.
  global[1] = C(5)
  println("4. local == \(local)")
  println("4. global[0] == \(global[0])")
  // CHECK-NEXT: 4. local == 4
  // CHECK-NEXT: 4. global[0] == 4

  // The connection is not yet broken.
  local = C(2)
  println("5. local == \(local)")
  println("5. global[0] == \(global[0])")
  // CHECK-NEXT: 5. local == 2
  // CHECK-NEXT: 5. global[0] == 2

  // This assignment structurally changes 'global' while a
  // simultaneous modification is occuring to it.  This is
  // allowed to have unspecified behavior but not to crash.
  global.append(C(3))
  println("6. local == \(local)")
  println("6. global[0] == \(global[0])")
  // CHECK-NEXT: 6. local == 2
  // CHECK-NEXT: 6. global[0] == 2

  // Note that here the connection is broken.
  local = C(7)
  println("7. local == \(local)")
  println("7. global[0] == \(global[0])")
  // CHECK-NEXT: 7. local == 7
  // CHECK-NEXT: 7. global[0] == 2
}
doit(&global[0])

println("8. global[0] == \(global[0])")
println("End")
// CHECK-NEXT: 8. global[0] == 2
// CHECK-NEXT: End