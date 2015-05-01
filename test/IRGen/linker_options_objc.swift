// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -target x86_64-unknown-linux-gnu -emit-module -parse-stdlib -o %t -module-name Empty -module-link-name swiftEmpty %S/../Inputs/empty.swift
// RUN: %swift -target x86_64-unknown-linux-gnu %s -I %t -parse-stdlib -disable-objc-interop -module-name main -emit-ir -o - | FileCheck %s

// REQUIRES: objc

import Empty

// Check that libobjc is always autolinked together with libswiftCore on
// platforms that support Objective-C.

// CHECK: {{.*}} = !{i32 6, !"Linker Options", !3}
// CHECK: {{.*}} = !{!"-lswiftCore"}
// CHECK: {{.*}} = !{!"-lobjc"}
