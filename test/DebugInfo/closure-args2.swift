// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func main () -> Void
{

    var random_string = "b"
    var random_int = 5

    var backward_ptr  =
        { (lhs : String, rhs : String) -> Bool in

    // CHECK-NOT: llvm.dbg.{{.*}}%swift.refcounted*
    // CHECK: !DILocalVariable(name: "lhs", arg: 1
    // CHECK: !DILocalVariable(name: "rhs", arg: 2
    // CHECK: !DILocalVariable(name: "random_string", arg: 3
    // CHECK: !DILocalVariable(name: "random_int", arg: 4

            if rhs == random_string
            || rhs.unicodeScalars.count == random_int {
                var local_var : Int64 = 10
                print("I have an int here \(local_var).\n", terminator: "")
                return false
            }
            else
            {
                var local_var : String = "g"
                print("I have another string here \(local_var).\n", terminator: "")
                return rhs < lhs
            }
        }

    var bool = backward_ptr("a" , "b")
}

main()

