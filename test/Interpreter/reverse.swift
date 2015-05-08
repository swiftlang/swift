// RUN: %target-run-simple-swift | FileCheck %s

func test()
{
    print("[", appendNewline: false)
    let reversed = lazy(0..<10).reverse()
    for i in reversed {
        print(i, appendNewline: false)
        print(" ", appendNewline: false)
    }
    print("]\n", appendNewline: false)
}

func testr()
{
    print("[", appendNewline: false)
    let reversed = lazy(0..<10).reverse()
    let doublyReversed = reversed.reverse()
    for i in doublyReversed {
        print(i, appendNewline: false)
        print(" ", appendNewline: false)
    }
    print("]\n", appendNewline: false)
}

test()
testr()

// CHECK: [9 8 7 6 5 4 3 2 1 0 ]
// CHECK: [0 1 2 3 4 5 6 7 8 9 ]
