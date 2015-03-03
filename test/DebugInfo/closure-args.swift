// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

import Swift

func main() -> Void
{
    // I am line 6.
    var random_string = "b"
    var random_int = 5
    var out_only = 2013

    var backward_ptr  =
    // CHECK: define linkonce_odr hidden i1 @_TFF4main4mainFT_T_U_FTSSSS_Sb(
    // CHECK: %[[RHS_ADDR:.*]] = alloca %SS*, align {{(4|8)}}
    // CHECK: store %SS* %{{.*}}, %SS** %[[RHS_ADDR]], align {{(4|8)}}
    // The shadow copying should happen in the prologue, because the
    // stack pointer will be decremented after it.
    // CHECK-NOT: !dbg
    // CHECK-NEXT: call void @llvm.dbg.declare(metadata %SS** %[[RHS_ADDR]], metadata !{{.*}}, metadata !{{[0-9]+}}), !dbg
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "lhs",{{.*}} line: [[@LINE+5]],
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "rhs",{{.*}} line: [[@LINE+4]],
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "random_string",{{.*}} line: 8,
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "random_int",{{.*}} line: 9,
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "out_only",{{.*}} line: 10,
        { (lhs : String, rhs : String) -> Bool in
            if rhs == random_string
               || count(rhs.unicodeScalars) == random_int
            {
            // Ensure the two local_vars are in different lexical scopes.
            // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "local_var", scope: ![[THENSCOPE:[0-9]+]],{{.*}} line: [[@LINE+2]],
            // CHECK-DAG: ![[THENSCOPE]] = distinct !MDLexicalBlock({{.*}} line: [[@LINE-3]]
                var local_var : Int = 10
                print ("I have an int here \(local_var).\n")
                return false
            }
            else
            {
            // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "local_var", scope: ![[ELSESCOPE:[0-9]+]],{{.*}} line: [[@LINE+2]]
            // CHECK-DAG: ![[ELSESCOPE]] = distinct !MDLexicalBlock({{.*}} line: [[@LINE-2]],
                var local_var : String = "g"
                print ("I have another string here \(local_var).\n")
                // Assign to all the captured variables to inhibit capture promotion.
                random_string = "c"
                random_int = -1
                out_only = 333
                return rhs < lhs
            }
        }

    var bool = backward_ptr("a" , "b")

    var my_string = ["a", "b", "c", "d"]

    var new_string = sorted (my_string, backward_ptr )

    print (new_string)
    print ("\n")
    print (random_int)
    print ("\n")
}

main()

