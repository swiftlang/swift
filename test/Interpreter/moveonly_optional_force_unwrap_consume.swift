// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

// Consuming a force-unwrapped noncopyable Optional, `x!`, must deinitialize the
// payload exactly once. Borrowing `x!` first must not change that.

struct Resource: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("deinit \(tag)") }

  borrowing func borrow() -> String { tag }
  mutating func mutate() {}
  consuming func close() { print("close \(tag)") }
}

@inline(never)
func make(_ tag: String) -> Resource? { Resource(tag) }

@inline(never)
func take(_ r: consuming Resource) { print("take \(r.tag)") }

struct Holder: ~Copyable {
  var resource: Resource?

  consuming func finish() {
    print(resource!.borrow())
    resource!.close()
  }
}

func iuoBorrowThenConsume() {
  var r: Resource!
  r = make("iuo")
  print(r.borrow())
  r.close()
}

func optionalBorrowThenConsume() {
  var r: Resource?
  r = make("optional")
  print(r!.borrow())
  r!.close()
}

func mutateThenConsume() {
  var r: Resource?
  r = make("mutated")
  r!.mutate()
  r!.close()
}

func consumeAsArgument() {
  var r: Resource?
  r = make("argument")
  take(r!)
}

func iuoConsumeAsArgument() {
  var r: Resource!
  r = make("iuo-argument")
  take(r)
}

func consumeMember() {
  let h = Holder(resource: make("member"))
  h.finish()
}

// CHECK: iuo
// CHECK-NEXT: close iuo
// CHECK-NEXT: deinit iuo
// CHECK-NEXT: optional
// CHECK-NEXT: close optional
// CHECK-NEXT: deinit optional
// CHECK-NEXT: close mutated
// CHECK-NEXT: deinit mutated
// CHECK-NEXT: take argument
// CHECK-NEXT: deinit argument
// CHECK-NEXT: take iuo-argument
// CHECK-NEXT: deinit iuo-argument
// CHECK-NEXT: member
// CHECK-NEXT: close member
// CHECK-NEXT: deinit member
// CHECK-NEXT: done
// CHECK-NOT: deinit
iuoBorrowThenConsume()
optionalBorrowThenConsume()
mutateThenConsume()
consumeAsArgument()
iuoConsumeAsArgument()
consumeMember()
print("done")
