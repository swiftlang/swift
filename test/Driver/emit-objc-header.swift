// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-objc-header-path %t/foo.h 2>&1 | %FileCheck %s

// CHECK: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-SAME: emit-objc-header.swift
// CHECK: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend -merge-modules
// CHECK-SAME: -emit-objc-header-path {{.+}}/foo.h
// CHECK: {{(bin/)?}}ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-objc-header-path %t/foo.h -whole-module-optimization 2>&1 | %FileCheck -check-prefix=CHECK-WHOLE-MODULE %s

// CHECK-WHOLE-MODULE: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-WHOLE-MODULE-SAME: emit-objc-header.swift
// CHECK-WHOLE-MODULE-SAME: -emit-objc-header-path {{.+}}/foo.h
// CHECK-WHOLE-MODULE-NOT: -merge-modules
// CHECK-WHOLE-MODULE: {{(bin/)?}}ld
