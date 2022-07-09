// RUN: %target-swift-frontend -emit-sil -g %s -o %t.sil
// RUN: %FileCheck %s --input-file=%t.sil
// Test the SILParser / SILPrinter
// RUN: %target-swift-frontend -module-name ImplicitVar -emit-sil -g %t.sil | %FileCheck %s

struct MyStruct {
    // Check if the 'implicit' directive for `self` is properly generated (and parsed)
    // CHECK: sil {{.*}} @{{.*}}MyStructV5hello
    // CHECK: debug_value %0 : $MyStruct
    // CHECK-SAME:        let, name "self", argno 1, implicit, loc
    func hello() {}
}
