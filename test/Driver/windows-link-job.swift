// RUN: env PATH= %swiftc_driver_plain -target x86_64-unknown-windows-msvc -### -module-name link -emit-library %s 2>&1 | %FileCheck %s
// CHECK: {{^}}clang++
