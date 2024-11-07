// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// ----------------------------------------
// Test the -profile-sample-use= flag using bogus data, to ensure it's actually
// reaching LLVM in the expected way.

// RUN: %target-swift-frontend %t/program.swift -module-name test -emit-ir \
// RUN:                  -O -profile-sample-use=%t/profile.txt -o %t/has-data.ll

// RUN: %FileCheck %s < %t/has-data.ll

// CHECK: define{{.*}} @"$s4test8anythingyyF"() #[[ATTRID:[0-9]+]]
// CHECK: attributes #[[ATTRID]] = {{.*}} "use-sample-profile"

// CHECK-LABEL: !llvm.module.flags
// CHECK: !{!"ProfileFormat", !"SampleProfile"}
// CHECK: !{!"TotalCount", i64 2001}

// ----------------------------------------
// Now, test cases where there there should not be any profile metadata,
// such as when no data is provided

// RUN: %target-swift-frontend %t/program.swift -module-name test -emit-ir \
// RUN:                        -O -o %t/no-data.ll

// RUN: %FileCheck --check-prefix CHECK-NODATA %s < %t/no-data.ll


// CHECK-NODATA: define{{.*}} @"$s4test8anythingyyF"() #[[ATTRID:[0-9]+]]
// CHECK-NODATA-NOT: attributes #[[ATTRID]] = {{.*}} "use-sample-profile"

// CHECK-NODATA-LABEL: !llvm.module.flags
// CHECK-NODATA-NOT: Profile


//--- program.swift
public func anything() {}


//--- profile.txt
bar:100:100
 1: 2001
