// RUN: %swiftc_driver -emit-executable -o %t.exe %s \
// RUN:   -enable-upcoming-feature MemberImportVisibility \
// RUN:   -enable-experimental-feature ParserValidation \
// RUN:   -disable-upcoming-feature MemberImportVisibility \
// RUN:   -disable-experimental-feature ParserValidation \
// RUN:   -disallow-use-new-driver -driver-print-jobs 2>&1 | %FileCheck %s

// The new driver has its own test for this

// REQUIRES: cplusplus_driver
// REQUIRES: swift_feature_ParserValidation
// REQUIRES: swift_feature_MemberImportVisibility

// CHECK: {{.*}}swift{{c|c-legacy-driver|-frontend}}{{(.exe)?"?}} -frontend
// CHECK-SAME: -enable-upcoming-feature MemberImportVisibility
// CHECK-SAME: -enable-experimental-feature ParserValidation
// CHECK-SAME: -disable-upcoming-feature MemberImportVisibility
// CHECK-SAME: -disable-experimental-feature ParserValidation
