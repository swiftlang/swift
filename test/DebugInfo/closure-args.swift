// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
import swift

func main () -> Void
{

    var random_string = "b"
    var random_int = 5

    var backward_ptr  =
    // CHECK: [ DW_TAG_arg_variable ] [rhs] [line [[@LINE+1]]]
        { (lhs : String, rhs : String) -> Bool in
            if rhs == random_string || rhs.length == random_int
            {
                var local_var : Int = 10
                print ("I have an int here \(local_var).\n")
                return false
            }
            else
            {
                var local_var : String = "g"
                print ("I have another string here \(local_var).\n")
                return rhs < lhs
            }
        }

    var bool = backward_ptr("a" , "b")

    var my_string = ["a", "b", "c", "d"]

    var new_string = sort (my_string, backward_ptr )

    print (new_string)
    print ("\n")
}

main()

