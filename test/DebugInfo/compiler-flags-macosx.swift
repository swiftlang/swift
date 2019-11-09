// Check that the sdk and resource dirs end up in the debug info if we build for
// a Darwin target and set the RC_DEBUG_OPTIONS environment variable. This
// matches the behavior found in Clang.

// RUN: %swiftc_driver %s -emit-ir -g -Xfrontend -disable-legacy-type-info -target x86_64-apple-macosx10.10 -parse-stdlib -module-name scratch -o - | %FileCheck %s
// RUN: env RC_DEBUG_OPTIONS=1 %swiftc_driver %s -emit-ir -g -Xfrontend -disable-legacy-type-info -target x86_64-apple-macosx10.10 -parse-stdlib -module-name scratch -o - | %FileCheck --check-prefix CHECK-VAR-SET %s
// CHECK:               !DICompileUnit({{.*}} producer: "{{[^"]*Swift version [^"]+}}"
// CHECK-NOT:                          flags: "
// CHECK-VAR-SET:       !DICompileUnit({{.*}}producer: "{{[^"]*Swift version [^"]+}}"
// CHECK-VAR-SET-SAME:                 flags: "
// CHECK-VAR-SET-NOT:                  "
// CHECK-VAR-SET-SAME:                 -resource-dir 
