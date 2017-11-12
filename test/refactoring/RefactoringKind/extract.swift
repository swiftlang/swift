func foo(i: Int, complete: ()->()) {}

func main() {
  foo(i: 2) {
    print(2)
  }
}

// RUN: %refactor -source-filename %s -pos=4:13 -end-pos=6:4 | %FileCheck %s -check-prefix=CHECK-NONE
// CHECK-NONE: Action begins
// CHECK-NONE-NEXT: Action ends
