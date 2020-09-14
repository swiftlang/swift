// RUN: %target-swift-frontend -import-objc-header %S/Inputs/local_extern.h %s -emit-ir | %FileCheck %s
// CHECK: @var = external {{(dso_local )?}}global i32
// CHECK: @prior_var = internal global i32
// CHECK: declare {{(dso_local )?}}i32 @func
// CHECK: define internal i32 @prior_func

print("\(_no_prior_var())")
print("\(_no_prior_func())")
print("\(_prior_var())")
print("\(_prior_func())")
