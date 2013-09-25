// RUN: rm -f %t.*
// RUN: %swift -emit-sil %s -o %t.sil
// RUN: FileCheck --input-file=%t.sil %s
// RUN: %swift -emit-silgen %t.sil -module-name=printer_include_decl | FileCheck %s

var x: Int
// CHECK: var x : Int

class Foo {
// FIXME: The constructors and destructors without bodies cannot be parsed.
/*  init(i:Int) { 
    self.x = i 
  }
  destructor { m() }*/
  
  var x : Int
// CHECK: var x : Int
  
  var y : Int {
    get: 
      return 5;
  }
// CHECK: var y : Int
  
  func m() {}
// CHECK: func m()
}

func bar(x : Foo) -> Int {
  return x.x
}
// CHECK-NOT: func bar(x : Foo) -> Int
