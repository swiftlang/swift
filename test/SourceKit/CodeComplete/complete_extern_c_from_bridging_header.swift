// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- Bridging-Header.h

void func_from_bridging_header(void);

extern "C"
void extern_c_func_from_bridging_header(void);

extern "C" {
  extern "C" {
    void nested_extern_c_func_from_bridging_header(void);
  }
}

//--- test.swift

// Passes
// RUN: %sourcekitd-test -req=complete -pos 1:1 %t/test.swift -- %t/test.swift -import-bridging-header %t/Bridging-Header.h -cxx-interoperability-mode=default -pch-output-dir %t/pch-dir | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos 1:1 %t/test.swift -- %t/test.swift -import-bridging-header %t/Bridging-Header.h -cxx-interoperability-mode=default | %FileCheck %s
// CHECK-DAG: func_from_bridging_header
// CHECK-DAG: extern_c_func_from_bridging_header
// CHECK-DAG: nested_extern_c_func_from_bridging_header
