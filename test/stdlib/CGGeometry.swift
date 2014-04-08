// RUN: %target-run-simple-swift | FileCheck %s

import CoreGraphics

func print(r: CGPoint, prefix: String) {
  println("\(prefix) \(r.x) \(r.y)")
}
func print(r: CGSize, prefix: String) {
  println("\(prefix) \(r.width) \(r.height)")
}

func print(r: CGVector, prefix: String) {
  println("\(prefix) \(r.dx) \(r.dy)")
}

func print(r: CGRect, prefix: String) {
  println("\(prefix) \(r.origin.x) \(r.origin.y) \(r.size.width) \(r.size.height)")
}

let int1: Int = 1
let int2: Int = 2
let int3: Int = 3
let int4: Int = 4

let double1: Double = 1
let double2: Double = 2
let double3: Double = 3
let double4: Double = 4

let float1: Float = 1
let float2: Float = 2
let float3: Float = 3
let float4: Float = 4

let cgfloat1: CGFloat = 1
let cgfloat2: CGFloat = 2
let cgfloat3: CGFloat = 3
let cgfloat4: CGFloat = 4

println("You may begin.")
// CHECK: You may begin.


var pt: CGPoint

pt = CGPoint(1.1, 2.2)
print(pt, "float literals")
pt = CGPoint(1, 2)
print(pt, "int literals")
pt = CGPoint(float1, float2)
print(pt, "floats")
pt = CGPoint(double1, double2)
print(pt, "doubles")
pt = CGPoint(cgfloat1, cgfloat2)
print(pt, "cgfloats")
pt = CGPoint(int1, int2)
print(pt, "ints")
// CHECK-NEXT: float literals 1.1 2.2
// CHECK-NEXT: int literals 1.0 2.0
// CHECK-NEXT: floats 1.0 2.0
// CHECK-NEXT: doubles 1.0 2.0
// CHECK-NEXT: cgfloats 1.0 2.0
// CHECK-NEXT: ints 1.0 2.0

pt = CGPoint(x:1.1, y:2.2)
print(pt, "named float literals")
pt = CGPoint(x:1, y:2)
print(pt, "named int literals")
pt = CGPoint(x:float1, y:float2)
print(pt, "named floats")
pt = CGPoint(x:double1, y:double2)
print(pt, "named doubles")
pt = CGPoint(x:cgfloat1, y:cgfloat2)
print(pt, "named cgfloats")
pt = CGPoint(x:int1, y:int2)
print(pt, "named ints")
// CHECK-NEXT: named float literals 1.1 2.2
// CHECK-NEXT: named int literals 1.0 2.0
// CHECK-NEXT: named floats 1.0 2.0
// CHECK-NEXT: named doubles 1.0 2.0
// CHECK-NEXT: named cgfloats 1.0 2.0
// CHECK-NEXT: named ints 1.0 2.0

assert(pt != CGPoint.zeroPoint)


var size: CGSize

size = CGSize(-1.1, -2.2)
print(size, "float literals")
size = CGSize(-1, -2)
print(size, "int literals")
size = CGSize(float1, float2)
print(size, "floats")
size = CGSize(double1, double2)
print(size, "doubles")
size = CGSize(cgfloat1, cgfloat2)
print(size, "cgfloats")
size = CGSize(int1, int2)
print(size, "ints")
// CHECK-NEXT: float literals -1.1 -2.2
// CHECK-NEXT: int literals -1.0 -2.0
// CHECK-NEXT: floats 1.0 2.0
// CHECK-NEXT: doubles 1.0 2.0
// CHECK-NEXT: cgfloats 1.0 2.0
// CHECK-NEXT: ints 1.0 2.0

size = CGSize(width:-1.1, height:-2.2)
print(size, "named float literals")
size = CGSize(width:-1, height:-2)
print(size, "named int literals")
size = CGSize(width:float1, height:float2)
print(size, "named floats")
size = CGSize(width:double1, height:double2)
print(size, "named doubles")
size = CGSize(width:cgfloat1, height:cgfloat2)
print(size, "named cgfloats")
size = CGSize(width:int1, height:int2)
print(size, "named ints")
// CHECK-NEXT: named float literals -1.1 -2.2
// CHECK-NEXT: named int literals -1.0 -2.0
// CHECK-NEXT: named floats 1.0 2.0
// CHECK-NEXT: named doubles 1.0 2.0
// CHECK-NEXT: named cgfloats 1.0 2.0
// CHECK-NEXT: named ints 1.0 2.0

assert(size != CGSize.zeroSize)


var vector: CGVector

vector = CGVector(-111.1, -222.2)
print(vector, "float literals")
vector = CGVector(-111, -222)
print(vector, "int literals")
vector = CGVector(float1, float2)
print(vector, "floats")
vector = CGVector(double1, double2)
print(vector, "doubles")
vector = CGVector(cgfloat1, cgfloat2)
print(vector, "cgfloats")
vector = CGVector(int1, int2)
print(vector, "ints")
// CHECK-NEXT: float literals -111.1 -222.2
// CHECK-NEXT: int literals -111.0 -222.0
// CHECK-NEXT: floats 1.0 2.0
// CHECK-NEXT: doubles 1.0 2.0
// CHECK-NEXT: cgfloats 1.0 2.0
// CHECK-NEXT: ints 1.0 2.0

vector = CGVector(dx:-111.1, dy:-222.2)
print(vector, "named float literals")
vector = CGVector(dx:-111, dy:-222)
print(vector, "named int literals")
vector = CGVector(dx:float1, dy:float2)
print(vector, "named floats")
vector = CGVector(dx:double1, dy:double2)
print(vector, "named doubles")
vector = CGVector(dx:cgfloat1, dy:cgfloat2)
print(vector, "named cgfloats")
vector = CGVector(dx:int1, dy:int2)
print(vector, "named ints")
// CHECK-NEXT: named float literals -111.1 -222.2
// CHECK-NEXT: named int literals -111.0 -222.0
// CHECK-NEXT: named floats 1.0 2.0
// CHECK-NEXT: named doubles 1.0 2.0
// CHECK-NEXT: named cgfloats 1.0 2.0
// CHECK-NEXT: named ints 1.0 2.0

assert(vector != CGVector.zeroVector)


var rect: CGRect

pt = CGPoint(10.1, 20.2)
size = CGSize(30.3, 40.4)
rect = CGRect(pt, size)
print(rect, "point+size")
rect = CGRect(origin:pt, size:size)
print(rect, "named point+size")
// CHECK-NEXT: point+size 10.1 20.2 30.3 40.4
// CHECK-NEXT: named point+size 10.1 20.2 30.3 40.4

rect = CGRect(10.1, 20.2, 30.3, 40.4)
print(rect, "float literals")
rect = CGRect(10, 20, 30, 40)
print(rect, "int literals")
rect = CGRect(float1, float2, float3, float4)
print(rect, "floats")
rect = CGRect(double1, double2, double3, double4)
print(rect, "doubles")
rect = CGRect(cgfloat1, cgfloat2, cgfloat3, cgfloat4)
print(rect, "cgfloats")
rect = CGRect(int1, int2, int3, int4)
print(rect, "ints")
// CHECK-NEXT: float literals 10.1 20.2 30.3 40.4
// CHECK-NEXT: int literals 10.0 20.0 30.0 40.0
// CHECK-NEXT: floats 1.0 2.0 3.0 4.0
// CHECK-NEXT: doubles 1.0 2.0 3.0 4.0
// CHECK-NEXT: cgfloats 1.0 2.0 3.0 4.0
// CHECK-NEXT: ints 1.0 2.0 3.0 4.0

rect = CGRect(x:10.1, y:20.2, width:30.3, height:40.4)
print(rect, "named float literals")
rect = CGRect(x:10, y:20, width:30, height:40)
print(rect, "named int literals")
rect = CGRect(x:float1, y:float2, width:float3, height:float4)
print(rect, "named floats")
rect = CGRect(x:double1, y:double2, width:double3, height:double4)
print(rect, "named doubles")
rect = CGRect(x:cgfloat1, y:cgfloat2, width:cgfloat3, height:cgfloat4)
print(rect, "named cgfloats")
rect = CGRect(x:int1, y:int2, width:int3, height:int4)
print(rect, "named ints")
// CHECK-NEXT: named float literals 10.1 20.2 30.3 40.4
// CHECK-NEXT: named int literals 10.0 20.0 30.0 40.0
// CHECK-NEXT: named floats 1.0 2.0 3.0 4.0
// CHECK-NEXT: named doubles 1.0 2.0 3.0 4.0
// CHECK-NEXT: named cgfloats 1.0 2.0 3.0 4.0
// CHECK-NEXT: named ints 1.0 2.0 3.0 4.0

assert(rect == rect)
assert(rect != CGRect.zeroRect)
assert(!rect.isNull)
assert(!rect.isEmpty)
assert(!rect.isInfinite)
assert(CGRect.nullRect.isNull)
assert(CGRect.zeroRect.isEmpty)
assert(CGRect.infiniteRect.isInfinite)


var unstandard = CGRect(10, 20, -30, -50)
var standard = unstandard.standardizedRect
print(unstandard, "unstandard")
print(standard, "standard")
// CHECK-NEXT: unstandard 10.0 20.0 -30.0 -50.0
// CHECK-NEXT: standard -20.0 -30.0 30.0 50.0

assert(unstandard.width == 30)
assert(unstandard.size.width == -30)
assert(standard.width == 30)
assert(standard.size.width == 30)

assert(unstandard.height == 50)
assert(unstandard.size.height == -50)
assert(standard.height == 50)
assert(standard.size.height == 50)

assert(unstandard.minX == -20)
assert(unstandard.midX == -5)
assert(unstandard.maxX == 10)

assert(unstandard.minY == -30)
assert(unstandard.midY == -5)
assert(unstandard.maxY == 20)

assert(unstandard == standard)
assert(unstandard.standardizedRect == standard)

unstandard.standardize()
print(unstandard, "standardized unstandard")
// CHECK-NEXT: standardized unstandard -20.0 -30.0 30.0 50.0


rect = CGRect(11.1, 22.2, 33.3, 44.4)
print(rect.rectByInsetting(1, -2), "rectByInsetting")
// CHECK-NEXT: rectByInsetting 12.1 20.2 31.3 48.4
rect.inset(1, -2)
print(rect, "inset")
// CHECK-NEXT: inset 12.1 20.2 31.3 48.4

rect = CGRect(11.1, 22.2, 33.3, 44.4)
print(rect.rectByOffsetting(3, -4), "rectByOffsetting")
// CHECK-NEXT: rectByOffsetting 14.1 18.2 33.3 44.4
rect.offset(3, -4)
print(rect, "offset")
// CHECK-NEXT: offset 14.1 18.2 33.3 44.4

rect = CGRect(11.1, 22.2, 33.3, 44.4)
print(rect.integerRect, "integerRect")
// CHECK-NEXT: integerRect 11.0 22.0 34.0 45.0
rect.integerize()
print(rect, "integerize")
// CHECK-NEXT: integerize 11.0 22.0 34.0 45.0


let smallRect = CGRect(10, 25, 5, -5)
let bigRect = CGRect(1, 2, 101, 102)
let distantRect = CGRect(1000, 2000, 1, 1)

rect = CGRect(11.1, 22.2, 33.3, 44.4)
print(rect.rectByUnion(smallRect), "rectByUnion small")
print(rect.rectByUnion(bigRect), "rectByUnion big")
print(rect.rectByUnion(distantRect), "rectByUnion distant")
// CHECK-NEXT: rectByUnion small 10.0 20.0 34.4 46.6
// CHECK-NEXT: rectByUnion big 1.0 2.0 101.0 102.0
// CHECK-NEXT: rectByUnion distant 11.1 22.2 989.9 1978.8
rect.union(smallRect)
rect.union(bigRect)
rect.union(distantRect)
print(rect, "union")
// CHECK-NEXT: union 1.0 2.0 1000.0 1999.0

rect = CGRect(11.1, 22.2, 33.3, 44.4)
print(rect.rectByIntersecting(smallRect), "rectByIntersecting small")
print(rect.rectByIntersecting(bigRect), "rectByIntersecting big")
print(rect.rectByIntersecting(distantRect), "rectByIntersecting distant")
// CHECK-NEXT: rectByIntersecting small 11.1 22.2 3.9 2.8
// CHECK-NEXT: rectByIntersecting big 11.1 22.2 33.3 44.4
// CHECK-NEXT: rectByIntersecting distant inf inf 0.0 0.0
assert(rect.intersects(smallRect))
rect.intersect(smallRect)
assert(!rect.isEmpty)
assert(rect.intersects(bigRect))
rect.intersect(bigRect)
assert(!rect.isEmpty)
assert(!rect.intersects(distantRect))
rect.intersect(distantRect)
assert(rect.isEmpty)


rect = CGRect(11.1, 22.2, 33.3, 44.4)
assert(rect.contains(CGPoint(15, 25)))
assert(!rect.contains(point:CGPoint(-15, 25)))
assert(bigRect.contains(rect))
assert(!rect.contains(rect:bigRect))


rect = CGRect(11.1, 22.2, 33.3, 44.4)
var (slice, remainder) = rect.rectsByDividing(atDistance:5, fromEdge:CGRectEdge.MinXEdge)
print(slice, "slice")
print(remainder, "remainder")
// CHECK-NEXT: slice 11.1 22.2 5.0 44.4
// CHECK-NEXT: remainder 16.1 22.2 28.3 44.4
