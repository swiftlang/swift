// RUN: %swift -i %s -- a b c d e | FileCheck %s

func printArgV() -> Int {
    for x in argv {
        print(x + " ")
    }
    return 0
}

var x : Int = printArgV()

// CHECK: a b c d e 
