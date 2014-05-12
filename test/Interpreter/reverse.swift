// RUN: %target-run-simple-swift | FileCheck %s

func test()
{
    print("[")
   for i in Reverse(0...10) {
        print(i)
        print(" ")
    }
    print("]\n")
}

func testr()
{
    print("[")
   for i in Reverse(Reverse(0...10)) {
        print(i)
        print(" ")
    }
    print("]\n")
}

test()
testr()

// CHECK: [9 8 7 6 5 4 3 2 1 0 ]
// CHECK: [0 1 2 3 4 5 6 7 8 9 ]
