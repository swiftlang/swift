// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

class Foo
{
  func DefinesClosure (a_string : String) -> () -> String
  {
    // Verify that we only emit the implicit argument,
    // and not the unowned local copy of self.
    //
    // CHECK-NOT: !DILocalVariable(name: "self", scope
    // CHECK: !DILocalVariable(name: "self", arg: 2
    // CHECK-NOT: !DILocalVariable(name: "self", scope
    return { [unowned self] in
             var tmp_string = a_string
             return tmp_string
           }
  }
}
