// RUN: %target-run-simple-swift | FileCheck %s

// Create a new array
var a = [Int](count: 10, repeatedValue: 0)
for i in 0..<10 { a[i] = i }
print(a)
// CHECK: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Create a new unsigned array
var u = [UInt64](count: 10, repeatedValue: 0)
for i in 0..<10 { u[i] = UInt64(i) }
print(u)
// CHECK: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice
a[1..<3] = a[5..<7]
print(a)
// CHECK: [0, 5, 6, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice with overlap
a[4..<8] = a[6..<10]
print(a)
// CHECK: [0, 5, 6, 3, 6, 7, 8, 9, 8, 9]

// Create another array and copy in a slice
var b = [Int](count: 10, repeatedValue: 0)
b[3...6] = a[5..<9]
print(b)
// CHECK: [0, 0, 0, 7, 8, 9, 8, 0, 0, 0]

// Create a 2D array
var aa = [[Int]](count: 10, repeatedValue: [])
for i in 0..<10 {
  var a = [Int](count: 10, repeatedValue: 0)
  for j in 0..<10 {
    a[j] = i*10 + j
  }
  aa[i] = a
}
print(aa)
// CHECK: {{\[}}[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [10, 11, 12, 13, 14, 15, 16, 17, 18, 19], [20, 21, 22, 23, 24, 25, 26, 27, 28, 29], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [40, 41, 42, 43, 44, 45, 46, 47, 48, 49], [50, 51, 52, 53, 54, 55, 56, 57, 58, 59], [60, 61, 62, 63, 64, 65, 66, 67, 68, 69], [70, 71, 72, 73, 74, 75, 76, 77, 78, 79], [80, 81, 82, 83, 84, 85, 86, 87, 88, 89], [90, 91, 92, 93, 94, 95, 96, 97, 98, 99]]

// Copy slices in a 2D array
aa[1..<3] = aa[2..<4]
print(aa)
// CHECK: {{\[}}[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [20, 21, 22, 23, 24, 25, 26, 27, 28, 29], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [40, 41, 42, 43, 44, 45, 46, 47, 48, 49], [50, 51, 52, 53, 54, 55, 56, 57, 58, 59], [60, 61, 62, 63, 64, 65, 66, 67, 68, 69], [70, 71, 72, 73, 74, 75, 76, 77, 78, 79], [80, 81, 82, 83, 84, 85, 86, 87, 88, 89], [90, 91, 92, 93, 94, 95, 96, 97, 98, 99]]


// Verify that array literals don't leak. <rdar://problem/16536439>
class Canary {
  deinit { print("dead") }
}

print("")

// CHECK: dead
// CHECK: dead
// CHECK: dead
_ = { [Canary(), Canary(), Canary()] }()

// Create an array of (String, Bool) pairs. <rdar://problem/16916422>
repeat {
  let x: [(String, Bool)] = [("foo", true)]
  print(x[0].0) // CHECK: foo
  print(x[0].1) // CHECK: true
} while false
print("still alive") // CHECK: still alive

// Construct some arrays and compare by equality.
let arr1 = [1, 2, 3, 4]
let arr2 = [1, 2, 3, 4]
let arr3 = [1, 2]
let arr4 = [5, 6]

let slice1_1_2 = arr1[1...2]
let slice2_1_2 = arr2[1...2]
let slice1_2_3 = arr1[2..<3]
let slice2_2_3 = arr2[2..<3]

let contig_arr1: ContiguousArray<Int> = [1, 2, 3, 4]
let contig_arr2: ContiguousArray<Int> = [1, 2, 3, 4]
let contig_arr3: ContiguousArray<Int> = [1, 2]
let contig_arr4: ContiguousArray<Int> = [5, 6]

// CHECK: arr1 == arr1: true
// CHECK-NEXT: arr1 == arr2: true
// CHECK-NEXT: arr1 != arr2: false
// CHECK-NEXT: arr2 == arr3: false
// CHECK-NEXT: arr2 != arr3: true
// CHECK-NEXT: arr1 != arr4: true
// CHECK-NEXT: arr1 == arr4: false
// CHECK-NEXT: slice1_1_2 == slice1_1_2: true
// CHECK-NEXT: slice1_1_2 == slice2_1_2: true
// CHECK-NEXT: slice1_1_2 != slice1_1_2: false
// CHECK-NEXT: slice1_2_3 == slice2_2_3: true
// CHECK-NEXT: contig_arr1 == contig_arr1: true
// CHECK-NEXT: contig_arr1 == contig_arr2: true
// CHECK-NEXT: contig_arr1 != contig_arr2: false
// CHECK-NEXT: contig_arr2 == contig_arr3: false
// CHECK-NEXT: contig_arr2 != contig_arr3: true
// CHECK-NEXT: contig_arr1 != contig_arr4: true
// CHECK-NEXT: contig_arr1 == contig_arr4: false
print("arr1 == arr1: \(arr1 == arr1)")
print("arr1 == arr2: \(arr1 == arr2)")
print("arr1 != arr2: \(arr1 != arr2)")
print("arr2 == arr3: \(arr2 == arr3)")
print("arr2 != arr3: \(arr2 != arr3)")
print("arr1 != arr4: \(arr1 != arr4)")
print("arr1 == arr4: \(arr1 == arr4)")
print("slice1_1_2 == slice1_1_2: \(slice1_1_2 == slice1_1_2)")
print("slice1_1_2 == slice2_1_2: \(slice1_1_2 == slice2_1_2)")
print("slice1_1_2 != slice1_1_2: \(slice1_1_2 != slice1_1_2)")
print("slice1_2_3 == slice2_2_3: \(slice1_2_3 == slice2_2_3)")
print("contig_arr1 == contig_arr1: \(contig_arr1 == contig_arr1)")
print("contig_arr1 == contig_arr2: \(contig_arr1 == contig_arr2)")
print("contig_arr1 != contig_arr2: \(contig_arr1 != contig_arr2)")
print("contig_arr2 == contig_arr3: \(contig_arr2 == contig_arr3)")
print("contig_arr2 != contig_arr3: \(contig_arr2 != contig_arr3)")
print("contig_arr1 != contig_arr4: \(contig_arr1 != contig_arr4)")
print("contig_arr1 == contig_arr4: \(contig_arr1 == contig_arr4)")

// <rdar://problem/16950035>
protocol Runcible {}
class Spoon: Runcible {
  let x: Int
  init(x: Int) { self.x = x }
}
let runceArray: [Runcible] = [Spoon(x: 219)]
// CHECK:      1 element
// CHECK-NEXT:   {{.*}}Spoon
// CHECK-NEXT:     x: 219
dump(runceArray)

// type check a large array literal in a reasonable amount of time
let afd: [Float] = [
    0.5, -0.5, -0.5,        1.0, 0.0, 0.0,
    0.5, 0.5, -0.5,         1.0, 0.0, 0.0,
    0.5, -0.5, 0.5,         1.0, 0.0, 0.0, 
    0.5, -0.5, 0.5,         1.0, 0.0, 0.0,
    0.5, 0.5, -0.5,         1.0, 0.0, 0.0,
    0.5, 0.5, 0.5,          1.0, 0.0, 0.0, 
    0.5, 0.5, -0.5,         0.0, 1.0, 0.0, 
    -0.5, 0.5, -0.5,        0.0, 1.0, 2.0
]

// Check equality on arrays
func test() {
  var a = [42]
  print(a == [42])
}
test()
// CHECK: true

