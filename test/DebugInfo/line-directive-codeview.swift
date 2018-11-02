func markUsed<T>(_ t: T) {}
func myFunc() {
  if 1==1 {
#sourceLocation(file: "abc.swift", line: 42)
    markUsed("Hello World")
#sourceLocation()
  }
  markUsed("Test")
#sourceLocation(file: "abc.swift", line: 142)
  markUsed("abc again")
#sourceLocation(file: "def.swift", line:  142)
  markUsed("jump directly to def")
}

// REQUIRES: OS=Windows
// RUN: %swiftc_driver %s -S -g -debug-info-format=codeview -target x86_64-unknown-windows-msvc -o - | %FileCheck --check-prefix CV-CHECK %s
// CV-CHECK: .cv_file [[MAIN:[0-9]+]] "{{.*}}line-directive-codeview.swift"
// CV-CHECK: .cv_loc {{[0-9]+}} [[MAIN]] 1 {{0?}}
// CV-CHECK: .def $S4main6myFuncyyF;
// CV-CHECK-NOT: .def
// CV-CHECK: .cv_func_id [[MYFUNC:[0-9]+]]
// CV-CHECK: .cv_file [[ABC:[0-9]+]] "{{.*}}abc.swift"
// CV-CHECK: .cv_loc [[MYFUNC]] [[ABC]] 42 {{0?}}
// CV-CHECK: .cv_loc [[MYFUNC]] [[MAIN]] 8 {{0?}}
// CV-CHECK: .cv_loc [[MYFUNC]] [[ABC]] 142 {{0?}}
// CV-CHECK: .cv_file [[DEF:[0-9]+]] "{{.*}}def.swift"
// CV-CHECK: .cv_loc [[MYFUNC]] [[DEF]] 142 {{0?}}
// CV-CHECK: .cv_linetable [[MYFUNC]], $S4main6myFuncyyF
