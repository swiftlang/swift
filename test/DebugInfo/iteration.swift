// RUN: %swift -target x86_64-apple-darwin %s -emit-ir -g -o - | FileCheck %s
var puzzleInput = "great minds think alike"
var puzzleOutput = ""
for letter in puzzleInput {
  // CHECK: [ DW_TAG_auto_variable ] [letter] [line [[@LINE-1]]]
    switch letter {
        case "a", "e", "i", "o", "u", " ":
            continue
        default:
            puzzleOutput += letter
    }
}
println(puzzleOutput)
