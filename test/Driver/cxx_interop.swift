// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -enable-cxx-interop 2>^1 | %FileCheck -check-prefix ENABLE %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -enable-cxx-interop -experimental-cxx-stdlib libc++ 2>^1 | %FileCheck -check-prefix STDLIB %s

// ENABLE: swift
// ENABLE: -enable-cxx-interop

// STDLIB: swift
// STDLIB-DAG: -enable-cxx-interop
// STDLIB-DAG: -Xcc -stdlib=libc++
