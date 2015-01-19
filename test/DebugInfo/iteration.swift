// RUN: %target-swift-frontend %s -emit-ir -g -o %t.ll
// RUN: FileCheck %s < %t.ll

var puzzleInput = "great minds think alike"
var puzzleOutput = ""
// CHECK-NOT: [ DW_TAG_auto_variable ] [$letter$generator] [line [[@LINE+2]]]
// CHECK: [ DW_TAG_auto_variable ] [letter] [line [[@LINE+1]]]
for letter in puzzleInput {
    switch letter {
        case "a", "e", "i", "o", "u", " ":
            continue
        default:
            puzzleOutput.append(letter)
    }
}
println(puzzleOutput)


func count() {
  // CHECK-NOT: [ DW_TAG_auto_variable ] [$i$generator] [line [[@LINE+2]]]
  // CHECK: [ DW_TAG_auto_variable ] [i] [line [[@LINE+1]]]
  for i in 0...100 {
    println(i)
  }
}
count()

// End-to-end test:
// RUN: llc %t.ll -filetype=obj -o - | llvm-dwarfdump - | FileCheck %s --check-prefix DWARF-CHECK
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "letter"
//
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "i"
