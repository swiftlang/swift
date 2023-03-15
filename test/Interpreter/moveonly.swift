// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

// CHECK: MyInt: 5
@inline(never)
func printInt(_ x: Int) { print("MyInt: \(x)") }

@_moveOnly
struct FD {
    var i = 5

    deinit {
        printInt(i)
    }
}

func main() {
    let x = FD()
    let _ = x
}

main()
