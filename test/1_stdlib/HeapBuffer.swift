// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift

print("testing...")
// CHECK: testing...

struct Bundle {
  init() {
    locations = Array()
  }
  var name = String()
  var locations: [String]
}

var a = _HeapBuffer<Bundle,Int>(_HeapBufferStorage<Bundle,Int>.self, Bundle(), 10)
var b = a.value
a.value.name = "DaveA"
a.value.locations.append("Princeton")
a.value.locations.append("San Jose")
for x in 0..<10 {
  (a.baseAddress + x).initialize(x)
}

print("buffer has storage: \(a.storage != nil)")
// CHECK-NEXT: buffer has storage: true

func testUnique() {
  print("buffer is unique: \(a.isUniquelyReferenced())")
  // CHECK-NEXT: buffer is unique: true
  
  var addRef = [ a ]
  print("copied buffer is unique: \(a.isUniquelyReferenced())")
  // CHECK-NEXT: copied buffer is unique: false
}
testUnique()

print("a == a: \(a == a)")
// CHECK-NEXT: a == a: true

let other = _HeapBuffer<Bundle,Int>(
  _HeapBufferStorage<Bundle,Int>.self, Bundle(), 0)
print("a == other: \(a == other)")
// CHECK-NEXT: a == other: false

print("name=\(a.value.name)")
// CHECK-NEXT: name=DaveA

print("length=\(a.value.locations.count)")
// CHECK-NEXT: length=2

print("locations[0]=\(a.value.locations[0])")
// CHECK-NEXT: locations[0]=Princeton

print("locations[1]=\(a.value.locations[1])")
// CHECK-NEXT: locations[1]=San Jose

for x in 0..<10 {
  print(a.baseAddress[x], appendNewline: false)
}
print("")
// CHECK-NEXT: 0123456789
