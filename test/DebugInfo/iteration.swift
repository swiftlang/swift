// RUN: %swift -triple x86_64-apple-darwin %s -emit-llvm -g -o - | FileCheck %s
var puzzleInput = "great minds think alike"
var puzzleOutput = ""
for letter in puzzleInput.chars {
  // CHECK: [ DW_TAG_auto_variable ] [letter] [line [[@LINE-1]]]
    switch letter {
        case 'a', 'e', 'i', 'o', 'u', ' ':
            continue
        default:
            puzzleOutput += letter
    }
}
println(puzzleOutput)
