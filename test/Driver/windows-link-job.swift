// RUN: %empty-directory(%t/DISTINCTIVE-WINDOWS-PATH/usr/bin)
// RUN: %empty-directory(%t/DISTINCTIVE-WINDOWS-PATH/usr/share/swift/diagnostics)
// RUN: cp -a %S/Inputs/diagnostics/. %t/DISTINCTIVE-WINDOWS-PATH/usr/share/swift/diagnostics
// RUN: %hardlink-or-copy(from: %swift_frontend_plain, to: %t/DISTINCTIVE-WINDOWS-PATH/usr/bin/swiftc)
// RUN: env PATH= %t/DISTINCTIVE-WINDOWS-PATH/usr/bin/swiftc -target x86_64-unknown-windows-msvc -### -module-name link -emit-library %s 2>&1 | %FileCheck %s
// CHECK: {{^}}clang
