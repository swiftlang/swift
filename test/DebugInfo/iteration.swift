// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s
var puzzleInput = "great minds think alike"
var puzzleOutput = ""
// CHECK: [ DW_TAG_auto_variable ] [$generator] [line [[@LINE+2]]]
// CHECK: [ DW_TAG_auto_variable ] [letter] [line [[@LINE+1]]]
for letter in puzzleInput {
    switch letter {
        case "a", "e", "i", "o", "u", " ":
            continue
        default:
            puzzleOutput += letter
    }
}
println(puzzleOutput)


func count() {
  // CHECK: [ DW_TAG_auto_variable ] [$generator] [line [[@LINE+2]]]
  // CHECK: [ DW_TAG_auto_variable ] [i] [line [[@LINE+1]]]
  for i in 0...100 {
    println(i)
  }
}
count()
