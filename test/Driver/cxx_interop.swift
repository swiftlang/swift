// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -Xfrontend -enable-experimental-cxx-interop 2>^1 | %FileCheck -check-prefix ENABLE %s

// ENABLE: swift
// ENABLE: -enable-experimental-cxx-interop
