struct Matrix {
  subscript(row: Int, column: Int) -> Int {
    return 0
  }
  
  subscript(row r: Int) -> [Int] {
    return []
  }
  
  subscript(column c: Int) -> [Int] {
    return []
  }
}

let matrix = Matrix()
matrix[]

// RUN: %sourcekitd-test -req=signaturehelp -pos=16:8 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.active_signature: 0,
// CHECK-NEXT:   key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "subscript(keyPath: KeyPath<Matrix, Value>) -> Value",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 10,
// CHECK-NEXT:           key.namelength: 31
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "subscript(row: Int, column: Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 10,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 20,
// CHECK-NEXT:           key.namelength: 11
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "subscript(row: Int) -> [Int]",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 10,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "subscript(column: Int) -> [Int]",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 10,
// CHECK-NEXT:           key.namelength: 11
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
