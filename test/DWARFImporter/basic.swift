// REQUIRES: executable_test
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// RUN: cp %S/Inputs/objc-header.h %S/Inputs/module.modulemap  %t
// RUN: %target-build-swift -emit-executable %s -g -o %t/a.out \
// RUN:   -module-name basic -emit-module -I%t
//        -DOBJC -module-name basic
// RUN: %lldb-moduleimport-test -verbose -dump-module %t/a.out | %FileCheck %s

// RUN: rm %t/objc-header.h
// RUN: %lldb-moduleimport-test -verbose -dump-module %t/a.out \
// RUN:    | %FileCheck %s --check-prefix=FAIL

// RUN: %lldb-moduleimport-test -verbose -dump-module %t/a.out \
// RUN:    -dummy-dwarfimporter | %FileCheck %s --check-prefix=SWIFTONLY

// CHECK: Importing basic...
// CHECK: Import successful!
// FAIL: Importing basic... 
// FAIL: Import successful!
// SWIFTONLY: Importing basic...
// SWIFTONLY: Import successful!

import ObjCModule

let pureSwift = Int32(42)
// FAIL-NOT:  var_decl
// CHECK:     var_decl {{.*}} "pureSwift"{{.*}} interface_type="Int32"
// SWIFTONLY: var_decl {{.*}} "pureSwift"{{.*}} interface_type="Int32" 

let point = Point(x: 1, y: 2)
// CHECK:     var_decl {{.*}} "point"{{.*}} interface_type="Point"
// SWIFTONLY-NOT: var_decl {{.*}} "point"

