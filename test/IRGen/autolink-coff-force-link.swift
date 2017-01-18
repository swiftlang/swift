// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift -target i686--windows-msvc -parse-as-library -parse-stdlib -module-name autolink -module-link-name autolink -autolink-force-load -emit-ir -o - %s | %FileCheck %s
// RUN: %swift -target i686--windows-itanium -parse-as-library -parse-stdlib -module-name autolink -module-link-name autolink -autolink-force-load -S -o - %s | %FileCheck %s -check-prefix CHECK-ASM-GNU
// RUN: %swift -target i686--windows-msvc -parse-as-library -parse-stdlib -module-name autolink -module-link-name autolink -autolink-force-load -S -o - %s | %FileCheck %s -check-prefix CHECK-ASM-MSC

// CHECK: @"_swift_FORCE_LOAD_$_autolink" = common dllexport global i1 false
// CHECK-ASM-GNU: .ascii  " -export:__swift_FORCE_LOAD_$_autolink,data"
// CHECK-ASM-MSC: .ascii  " /EXPORT:__swift_FORCE_LOAD_$_autolink,DATA"

