// RUN: %target-swift-emit-silgen -parse-as-library %s -module-name A | %FileCheck %s

func foo(_ a: _const Int) {}
func bar(a: _const String) {}

// CHECK: @$s1A3fooyySiYtF
// CHECK: @$s1A3bar1aySSYt_tF

// RUN: %swift-demangle s1A3fooyySiYtF | %FileCheck %s -check-prefix=CHECK-FOO

// CHECK-FOO: A.foo(_const Swift.Int) -> ()

// RUN: %swift-demangle s1A3bar1aySSYt_tF | %FileCheck %s -check-prefix=CHECK-BAR

// CHECK-BAR: A.bar(a: _const Swift.String) -> ()
