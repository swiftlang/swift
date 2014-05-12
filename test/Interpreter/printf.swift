// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: |              Hello       5BBF    3.14159|
printf("|%19v %10X %10v|\n", "Hello", 23487, 3.14159)

// CHECK: |Hello                     5BBF    3.14159|
printf("|%-19v %10X %10v|\n", "Hello", 23487, 3.14159)

// CHECK: |HELLO                     5BBF    3.14159|
printf("|%-19u %10X %10v|\n", "Hello", 23487, 3.14159)

