// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 0]
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 8]
// CHECK: [ DW_TAG_arg_variable ] [a] [line 10] [piece, size 8, offset 16]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 0]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 8]
// CHECK: [ DW_TAG_arg_variable ] [b] [line 10] [piece, size 8, offset 16]
func demo () {
    var names = ["Sean", "Barry", "Kate"]
    var sortedNames = sort(names) {(a, b) in
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
