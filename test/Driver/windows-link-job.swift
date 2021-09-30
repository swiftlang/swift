// RUN: %empty-directory(%t/DISTINCTIVE-WINDOWS-PATH/usr/bin)
// RUN: %hardlink-or-copy(from: %swift_frontend_plain, to: %t/DISTINCTIVE-WINDOWS-PATH/usr/bin/swiftc)
// RUN: env PATH= %t/DISTINCTIVE-WINDOWS-PATH/usr/bin/swiftc -target x86_64-unknown-windows-msvc -### -module-name link -emit-library %s 2>&1 | %FileCheck %s

// swift-frontend cannot be copied to another location with bootstrapping because
// it will not find the libswiftCore library with its relative RPATH.
// UNSUPPORTED: libswift_bootstrapping

// CHECK: {{^}}clang
