// RUN: %swift -i %s | FileCheck %s

struct Bundle {
  constructor() {
    locations = Vector()
  }
  var name: String
  var locations: Vector<String>
}

var a = HeapBuffer<Bundle,Int>.create(Bundle(), 10)
var b = a.value
a.value.name = "DaveA"
a.value.locations.append("Princeton")
a.value.locations.append("San Jose")
for x in 0..10 {
  (a.elementStorage + x).init(x)
}

println("name=\(a.value.name)")
// CHECK: name=DaveA

println("length=\(a.value.locations.length)")
// CHECK: length=2

println("locations[0]=\(a.value.locations[0])")
// CHECK: locations[0]=Princeton

println("locations[1]=\(a.value.locations[1])")
// CHECK: locations[1]=San Jose

for x in 0..10 {
  print(a.elementStorage[x])
}
println("")
// CHECK: 0123456789
