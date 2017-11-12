// RUN: %swift -interpret %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g -- | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g -- a b c | %FileCheck %s -check-prefix=CHECK-THREE

// REQUIRES: swift_interpreter

print("Begin arguments")
for arg in CommandLine.arguments { print(arg) }
print("End arguments")

// CHECK-NONE: Begin arguments
// CHECK-NONE-NEXT: {{.*}}process_arguments.swift
// CHECK-NONE-NEXT: End arguments

// CHECK-THREE: Begin arguments
// CHECK-THREE-NEXT: {{.*}}process_arguments.swift
// CHECK-THREE-NEXT: a
// CHECK-THREE-NEXT: b
// CHECK-THREE-NEXT: c
// CHECK-THREE-NEXT: End arguments

print("Begin unsafeArgv")
for i in 0...Int(CommandLine.argc) {
  print(CommandLine.unsafeArgv[i].map { String(cString: $0) } ?? "(null)")
}
print("End unsafeArgv")

// CHECK-NONE: Begin unsafeArgv
// CHECK-NONE-NEXT: {{.*}}process_arguments.swift
// CHECK-NONE-NEXT: (null)
// CHECK-NONE-NEXT: End unsafeArgv

// CHECK-THREE: Begin unsafeArgv
// CHECK-THREE-NEXT: {{.*}}process_arguments.swift
// CHECK-THREE-NEXT: a
// CHECK-THREE-NEXT: b
// CHECK-THREE-NEXT: c
// CHECK-THREE-NEXT: (null)
// CHECK-THREE-NEXT: End unsafeArgv
