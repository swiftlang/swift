// RUN: %swift -target x86_64-apple-macosx10.9 -O %s -emit-ir -g -o - | FileCheck %s
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 0]
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 8]
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 16]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 0]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 8]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 16]
func demo () {
    var names = ["Sean", "Barry", "Kate"]
    var sortedNames = sorted(names) {(a, b) in
        println("Sorting..\(a) & \(b)")
        return (a < b)
    }
    var sortedNamesAsString : String = String()
    for name in sortedNames {
        sortedNamesAsString += ("\(name), ")
    }
    println(sortedNamesAsString)
}
demo()

// At -O0, we should have a single aggregate argument.
// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s --check-prefix=CHECK-O0
// CHECK-O0: [ DW_TAG_arg_variable ] [a] [line 10]
// CHECK-O0-NOT: piece
// CHECK-O0: [ DW_TAG_arg_variable ] [b] [line 10]
// CHECK-O0-NOT: piece
