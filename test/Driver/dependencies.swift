// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -parse -parse-as-library -o %t -emit-dependencies %s %S/Inputs/empty.swift -I=%S/Inputs
// RUN: FileCheck %s < %t/dependencies.d
// RUN: %swift -parse -parse-as-library -o %t/abc -emit-dependencies %s %S/Inputs/empty.swift -I=%S/Inputs
// RUN: FileCheck %s -check-prefix=ABC < %t/abc.d
// RUN: %swift -emit-silgen -parse-as-library -o %t/test.sil -emit-dependencies %s %S/Inputs/empty.swift -I=%S/Inputs
// RUN: FileCheck %s -check-prefix=SIL < %t/test.d

import single_int

// CHECK: dependencies.o : {{.*}}dependencies.swift {{.*}}empty.swift
// CHECK-DAG: single_int.swift
// CHECK-DAG: swift.swiftmodule

// ABC: abc.o : {{.*}}dependencies.swift {{.*}}empty.swift
// ABC-DAG: single_int.swift
// ABC-DAG: swift.swiftmodule

// SIL: test.sil : {{.*}}dependencies.swift {{.*}}empty.swift
// SIL-DAG: single_int.swift
// SIL-DAG: swift.swiftmodule
