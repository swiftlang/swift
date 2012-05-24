// RUN: %swift %s -i | FileCheck %s

// CHECK: |              Hello       5BBF    3.14159|
printf("|%19v %10x %10v|\n", "Hello", 23487, 3.14159)

// CHECK: |Hello                     5BBF    3.14159|
printf("|%-19v %10x %10v|\n", "Hello", 23487, 3.14159)

