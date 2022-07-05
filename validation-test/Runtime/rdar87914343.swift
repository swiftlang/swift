// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:     -emit-module \
// RUN:     %S/Inputs/rdar87914343_Resilient.swift \
// RUN:     -parse-as-library \
// RUN:     -enable-library-evolution \
// RUN:     -module-name Resilient \
// RUN:     -emit-module-path %t/Resilient.swiftmodule

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %S/Inputs/rdar87914343_Resilient.swift \
// RUN:     -parse-as-library \
// RUN:     -enable-library-evolution \
// RUN:     -module-name Resilient \
// RUN:     -o %t/Resilient.o

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %s \
// RUN:     -o %t/main.o \
// RUN:     -I %t

// RUN: %target-swiftc_driver \
// RUN:     %t/Resilient.o \
// RUN:     %t/main.o \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Resilient

func dump<T>(_ value: T) {
  print(value)
}

enum ProblematicEnumeration {
    case zero(ResilientEnum)
    case one(Bool)
    case two
    case three
    case four
    case five
    case six
}

func doit() {
    dump(ProblematicEnumeration.six)
}

doit()
// CHECK: six
