// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:     -emit-module \
// RUN:     %S/Inputs/rdar79513293_Library.swift \
// RUN:     -parse-as-library \
// RUN:     -module-name Library \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %S/Inputs/rdar79513293_Library.swift \
// RUN:     -parse-as-library \
// RUN:     -module-name Library \
// RUN:     -o %t/Library.o

// RUN: %target-build-swift \
// RUN:     -emit-module \
// RUN:     %S/Inputs/rdar79513293_Resilient.swift \
// RUN:     -parse-as-library \
// RUN:     -enable-library-evolution \
// RUN:     -module-name Resilient \
// RUN:     -emit-module-path %t/Resilient.swiftmodule

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %S/Inputs/rdar79513293_Resilient.swift \
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
// RUN:     %t/Library.o \
// RUN:     %t/main.o \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

import Library
import Resilient

protocol P {}

struct I: P {
  var s = S<Unfixed?>()
}

func main() {
  let ps = [I() as P]
  for _ in ps {}
}

main()
print("ok")
// CHECK: ok
