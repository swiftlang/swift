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

let cgfloat1: CGFloat = 1
let cgfloat2: CGFloat = 2
let cgfloat3: CGFloat = 3
let cgfloat4: CGFloat = 4

println("You may begin.")
// CHECK: You may begin.


var pt: CGPoint

pt = CGPoint(x: 1.25, y: 2.25)
print(pt, "named float literals")
pt = CGPoint(x: 1, y: 2)
print(pt, "named int literals")
pt = CGPoint(x: cgfloat1, y: cgfloat2)
print(pt, "named cgfloats")
pt = CGPoint(x: int1, y: int2)
print(pt, "named ints")
// CHECK-NEXT: named float literals 1.25 2.25
// CHECK-NEXT: named int literals 1 2
// CHECK-NEXT: named cgfloats 1 2
// CHECK-NEXT: named ints 1 2

assert(pt != CGPoint.zeroPoint)


var size: CGSize

size = CGSize(width:-1.25, height:-2.25)
print(size, "named float literals")
size = CGSize(width:-1, height:-2)
print(size, "named int literals")
size = CGSize(width:cgfloat1, height:cgfloat2)
print(size, "named cgfloats")
size = CGSize(width: int1, height: int2)
print(size, "named ints")
// CHECK-NEXT: named float literals -1.25 -2.25
// CHECK-NEXT: named int literals -1 -2
// CHECK-NEXT: named cgfloats 1 2
// CHECK-NEXT: named ints 1 2

assert(size != CGSize.zeroSize)


var vector: CGVector

vector = CGVector(-111.25, -222.25)
print(vector, "float literals")
vector = CGVector(-111, -222)
print(vector, "int literals")
vector = CGVector(cgfloat1, cgfloat2)
print(vector, "cgfloats")
vector = CGVector(int1, int2)
print(vector, "ints")
// CHECK-NEXT: float literals -111.25 -222.25
// CHECK-NEXT: int literals -111 -222
// CHECK-NEXT: cgfloats 1 2
// CHECK-NEXT: ints 1 2

vector = CGVector(-111.25, -222.25)
print(vector, "named float literals")
vector = CGVector(-111, -222)
print(vector, "named int literals")
vector = CGVector(cgfloat1, cgfloat2)
print(vector, "named cgfloats")
vector = CGVector(int1, int2)
print(vector, "named ints")
// CHECK-NEXT: named float literals -111.25 -222.25
// CHECK-NEXT: named int literals -111 -222
// CHECK-NEXT: named cgfloats 1 2
// CHECK-NEXT: named ints 1 2

assert(vector != CGVector.zeroVector)


var rect: CGRect

pt = CGPoint(x: 10.25, y: 20.25)
size = CGSize(width: 30.25, height: 40.25)
rect = CGRect(origin: pt, size: size)
print(rect, "point+size")
rect = CGRect(origin:pt, size:size)
print(rect, "named point+size")
// CHECK-NEXT: point+size 10.25 20.25 30.25 40.25
// CHECK-NEXT: named point+size 10.25 20.25 30.25 40.25

rect = CGRect(x:10.25, y:20.25, width:30.25, height:40.25)
print(rect, "named float literals")
rect = CGRect(x:10, y:20, width:30, height:40)
print(rect, "named int literals")
rect = CGRect(x:cgfloat1, y:cgfloat2, width:cgfloat3, height:cgfloat4)
print(rect, "named cgfloats")
rect = CGRect(x:int1, y:int2, width:int3, height:int4)
print(rect, "named ints")
// CHECK-NEXT: named float literals 10.25 20.25 30.25 40.25
// CHECK-NEXT: named int literals 10 20 30 40
// CHECK-NEXT: named cgfloats 1 2 3 4
// CHECK-NEXT: named ints 1 2 3 4

assert(rect == rect)
assert(rect != CGRect.zeroRect)
assert(!rect.isNull)
assert(!rect.isEmpty)
assert(!rect.isInfinite)
assert(CGRect.nullRect.isNull)
assert(CGRect.zeroRect.isEmpty)
assert(CGRect.infiniteRect.isInfinite)


var unstandard = CGRect(x: 10, y: 20, width: -30, height: -50)
var standard = unstandard.standardizedRect
print(unstandard, "unstandard")
print(standard, "standard")
// CHECK-NEXT: unstandard 10 20 -30 -50
// CHECK-NEXT: standard -20 -30 30 50

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
// CHECK-NEXT: standardized unstandard -20 -30 30 50


rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
print(rect.rectByInsetting(dx: 1, dy: -2), "rectByInsetting")
// CHECK-NEXT: rectByInsetting 12.25 20.25 31.25 48.25
rect.inset(dx: 1, dy: -2)
print(rect, "inset")
// CHECK-NEXT: inset 12.25 20.25 31.25 48.25

rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
print(rect.rectByOffsetting(dx: 3, dy: -4), "rectByOffsetting")
// CHECK-NEXT: rectByOffsetting 14.25 18.25 33.25 44.25
rect.offset(dx: 3, dy: -4)
print(rect, "offset")
// CHECK-NEXT: offset 14.25 18.25 33.25 44.25

rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
print(rect.integerRect, "integerRect")
// CHECK-NEXT: integerRect 11 22 34 45
rect.integerize()
print(rect, "integerize")
// CHECK-NEXT: integerize 11 22 34 45


let smallRect = CGRect(x: 10, y: 25, width: 5, height: -5)
let bigRect = CGRect(x: 1, y: 2, width: 101, height: 102)
let distantRect = CGRect(x: 1000, y: 2000, width: 1, height: 1)

rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
print(rect.rectByUnion(smallRect), "rectByUnion small")
print(rect.rectByUnion(bigRect), "rectByUnion big")
print(rect.rectByUnion(distantRect), "rectByUnion distant")
// CHECK-NEXT: rectByUnion small 10 20 34.5 46.5
// CHECK-NEXT: rectByUnion big 1 2 101 102
// CHECK-NEXT: rectByUnion distant 11.25 22.25 989.75 1978.75
rect.union(smallRect)
rect.union(bigRect)
rect.union(distantRect)
print(rect, "union")
// CHECK-NEXT: union 1 2 1000 1999

rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
print(rect.rectByIntersecting(smallRect), "rectByIntersecting small")
print(rect.rectByIntersecting(bigRect), "rectByIntersecting big")
print(rect.rectByIntersecting(distantRect), "rectByIntersecting distant")
// CHECK-NEXT: rectByIntersecting small 11.25 22.25 3.75 2.75
// CHECK-NEXT: rectByIntersecting big 11.25 22.25 33.25 44.25
// CHECK-NEXT: rectByIntersecting distant inf inf 0 0
assert(rect.intersects(smallRect))
rect.intersect(smallRect)
assert(!rect.isEmpty)
assert(rect.intersects(bigRect))
rect.intersect(bigRect)
assert(!rect.isEmpty)
assert(!rect.intersects(distantRect))
rect.intersect(distantRect)
assert(rect.isEmpty)


rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
assert(rect.contains(CGPoint(x: 15, y: 25)))
assert(!rect.contains(CGPoint(x: -15, y: 25)))
assert(bigRect.contains(rect))
assert(!rect.contains(bigRect))


rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
var (slice, remainder) = rect.rectsByDividing(5, fromEdge:CGRectEdge.MinXEdge)
print(slice, "slice")
print(remainder, "remainder")
// CHECK-NEXT: slice 11.25 22.25 5 44.25
// CHECK-NEXT: remainder 16.25 22.25 28.25 44.25
