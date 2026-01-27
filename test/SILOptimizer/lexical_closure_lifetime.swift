// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

class C {}

func runit<T>(_ c: () -> T) -> T {
  return c()
}

func testit() -> C? {
    weak var weakRef: C? = nil

    let resultsHandler = runit {
        let c = C()
        weakRef = c

        let x: () -> () = {
            _ = c
        }
        return x
    }

    resultsHandler()

    return weakRef
}

if testit() == nil {
  fatalError()
}

// CHECK: success
print("success")

