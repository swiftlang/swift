// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func main () -> Void
{

    var random_string = "b"
    var random_int = 5

    var backward_ptr  =
        { (lhs : String, rhs : String) -> Bool in

    // CHECK-NOT: llvm.dbg.{{.*}}%swift.refcounted*
    // CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "lhs"
    // CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "rhs"
    // CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "random_string"
    // CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "random_int"

            if rhs == random_string
            || rhs.unicodeScalars.count == random_int {
                var local_var : Int = 10
                print("I have an int here \(local_var).\n", appendNewline: false)
                return false
            }
            else
            {
                var local_var : String = "g"
                print("I have another string here \(local_var).\n", appendNewline: false)
                return rhs < lhs
            }
        }

    var bool = backward_ptr("a" , "b")
}

main()

