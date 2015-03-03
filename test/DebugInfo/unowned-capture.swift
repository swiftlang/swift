// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

class Foo
{
  func DefinesClosure (a_string : String) -> () -> String
  {
    // Verify that we only emit the implicit argument,
    // and not the unowned local copy of self.
    //
    // CHECK-NOT: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "self"
    // CHECK: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "self"
    // CHECK-NOT: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "self"
    return { [unowned self] in
             var tmp_string = a_string
             return tmp_string
           }
  }
}
