// RUN: %empty-directory(%t)
// RUN: echo 'struct Second {}' > %t/second.swift
// RUN: %target-swift-frontend -c -O -num-threads 2 -module-name Test %s %t/second.swift -o %t/first.o -o %t/second.o -emit-dependencies-path %t/default.d
// RUN: %FileCheck %s --check-prefix=DEFAULT < %t/default.d
// RUN: %target-swift-frontend -c -O -num-threads 2 -module-name Test %s %t/second.swift -o %t/first.o -o %t/second.o -emit-dependencies-path %t/first-only.d -emit-dependencies-first-output-only
// RUN: head -n 1 %t/default.d > %t/expected.d
// RUN: diff -u %t/expected.d %t/first-only.d
// RUN: %target-swift-frontend -c -module-name Test %s -o %t/single.o -emit-dependencies-path %t/single-default.d
// RUN: %target-swift-frontend -c -module-name Test %s -o %t/single.o -emit-dependencies-path %t/single-first-only.d -emit-dependencies-first-output-only
// RUN: diff -u %t/single-default.d %t/single-first-only.d

// DEFAULT: first.o : {{.*}}emit-dependencies-first-output-only.swift{{.*}}second.swift
// DEFAULT-NEXT: second.o : {{.*}}emit-dependencies-first-output-only.swift{{.*}}second.swift

struct First {}
