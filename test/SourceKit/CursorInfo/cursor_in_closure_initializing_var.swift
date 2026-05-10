// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- %s | %FileCheck %s

class MyClass {
    lazy var calculatorContext: Int = {
        let context = 1
        return context
    }()
}

// CHECK: s:34cursor_in_closure_initializing_var7MyClassC17calculatorContextSivgSiyXEfU_7contextL_Sivp
