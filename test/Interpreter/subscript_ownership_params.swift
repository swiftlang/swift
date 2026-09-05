// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Ownership specifiers on subscript parameters, at runtime.
//
// An `inout` index takes the exclusive access itself, so the accessors of one
// formal access all mutate through the same access -- a mutation an accessor
// makes to the index is visible to the caller afterwards.

struct Table {
  var slots: [Int] = [10, 20, 30]

  // Advances the index on a write, so the caller can observe the mutation.
  subscript(i: inout Int) -> Int {
    get { return slots[i] }
    set { slots[i] = newValue; i += 1 }
  }

  subscript(coro i: inout Int) -> Int {
    _read { yield slots[i] }
    _modify { i += 1; yield &slots[i] }
  }
}

var t = Table()
var i = 0

// A get does not run the setter, so the index is untouched.
// CHECK: get 10 0
print("get", t[&i], i)

// A set runs only the setter, which advances the index.
i = 0
t[&i] = 99
// CHECK-NEXT: set [99, 20, 30] 1
print("set", t.slots, i)

// A read-modify-write runs the getter and then the setter over one access.
i = 0
t[&i] += 5
// CHECK-NEXT: rmw [104, 20, 30] 1
print("rmw", t.slots, i)

// The same through a coroutine accessor pair.
i = 0
// CHECK-NEXT: read 104 0
print("read", t[coro: &i], i)

i = 0
t[coro: &i] += 1
// CHECK-NEXT: modify [104, 21, 30] 1
print("modify", t.slots, i)

// A noncopyable index, which is what an `inout` index makes possible along with
// `borrowing`: it is neither copied nor consumed.
struct Counter: ~Copyable {
  var value: Int
}

struct NCTable {
  var slots: [Int] = [1, 2, 3]
  subscript(c: inout Counter) -> Int {
    get { return slots[c.value] }
    set { slots[c.value] = newValue; c.value += 1 }
  }
}

var nct = NCTable()
var counter = Counter(value: 0)
// CHECK-NEXT: nc get 1
print("nc get", nct[&counter])
nct[&counter] = 42
// CHECK-NEXT: nc set [42, 2, 3] 1
print("nc set", nct.slots, counter.value)
nct[&counter] += 8
// CHECK-NEXT: nc rmw [42, 10, 3] 2
print("nc rmw", nct.slots, counter.value)

// A `borrowing` index is not copied and stays usable afterwards.
struct BTable {
  var slots: [Int] = [7, 8]
  subscript(c: borrowing Counter) -> Int {
    get { return slots[c.value] }
    set { slots[c.value] = newValue }
  }
}

var bt = BTable()
let borrowed = Counter(value: 1)
bt[borrowed] += 1
// CHECK-NEXT: borrowing [7, 9] 1
print("borrowing", bt.slots, borrowed.value)
