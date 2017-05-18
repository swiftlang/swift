// REQUIRES: OS=macosx
// Note: This is really about the /host/ environment

// RUN: %swiftc_driver -driver-print-jobs -static-stdlib %S/../Inputs/empty.swift | %FileCheck -check-prefix STATIC %s
// RUN: %swiftc_driver -driver-print-jobs %S/../Inputs/empty.swift | %FileCheck -check-prefix NO_STATIC %s

// STATIC: {{.*}}-L {{[^ ]*}}/lib/swift_static/macosx {{.*}}-licucore 
// NO_STATIC: {{.*}}-L {{[^ ]*}}/lib/swift/macosx
