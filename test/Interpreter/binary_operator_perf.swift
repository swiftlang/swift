// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

var test1 =
"hello" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world" +
"world"

print(test1)

// CHECK: helloworldworldworldworldworldworldworldworldworldworldworldworldworldworld

var test2 =
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 /*+ // 100
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 200
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 300
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 400
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 500
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 600
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 700
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 800
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + // 900
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 // 1000
*/

print(test2)
// CHECK: 100

print("\(1.0/1.0) \(1.0/1.0) \(1.0/1.0)")
// CHECK: 1.0 1.0 1.0

var yo = "yo"
var yoyo = yo + yo + yo + yo + yo + yo + yo + yo
print(yoyo)
// CHECK: yoyoyoyoyoyoyoyo

var i1 = 1
var ia = i1 + i1 + i1 + i1 + i1 + i1 + i1 + i1 + i1 + i1
print(ia)
// CHECK: 10

print( "test" + "ing" + " " + "this" + " " + "code" )
// CHECK: testing this code

let at1 : Float = 0
let at2 : Float = 0
let rat : Float = 0
var rr : Float = 0
var ii : Float = 0
print((rr-at1)*(rr-at1) + (ii-at2)*(ii-at2))
// CHECK: 0.0

let y = 1
let kWorldTileDivisor = 32
let kWorldSize = 4096
let kWorldTileSize = kWorldSize / kWorldTileDivisor
let kWorldCenter = kWorldSize / 2

print((kWorldSize - (y * kWorldTileSize)) - kWorldCenter  - kWorldTileSize / 2)
// CHECK: 1856

let p: [Double] = [0.0, 1.0]
let q: [Double] = [0.0, 1.0]
let r: [Double] = [0.0, 1.0]
let size : Double = (q[0]-p[0])*(r[1]-p[1]) - (q[1]-p[1])*(r[0]-p[0])
print(size)
// CHECK: 0.0

let cc = 1 + 1 + 1.0 + 1 + 1 + 1.0
print(cc)
// CHECK: 6.0

let dd = 1 + 1 + Double(1) + 1 + Double(1)
print(dd)
// CHECK: 5.0

struct Point2D {
	var x: Float
	var y: Float
}

let points: (Point2D, Point2D, Point2D) = ( Point2D(x: 1.0, y:1.0),
											Point2D(x: 1.0, y: 1.0), 
											Point2D(x: 1.0, y: 1.0))

var signedArea: Float {
    get {
        let (a, b, c) = points
        let signedArea = 0.5 * (
            a.x * (b.y - c.y) +
            b.x * (c.y - a.y) +
            c.x * (a.y - b.y)
        )
        return signedArea
    }
}

print(signedArea)
// CHECK: 0.0

struct D {
  var year: Int
  var month: Int
  var day: Int
  init(year: Int, month: Int, day: Int) {
    self.year = year
    self.month = month
    self.day = day
  }
  init?(s: String) {
    return nil
  }
}
let components = D(year:1776, month:7, day:4)
let date = String(components.year) + "-" + String(components.month) + "-" + String(components.day)
print(date)
// CHECK: 1776-7-4
