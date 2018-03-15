// When multiple additional-outputs on the same command-line are no longer
// supported (i.e. when we've moved to mandatory use of output file maps for
// communicating multiple additional-outputs to frontends) this test will no
// longer make sense, and should be removed.
//
// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift
// RUN: touch %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift
//
// RUN: %swiftc_driver -enable-batch-mode -driver-batch-seed 1 -driver-print-jobs -driver-skip-execution -j 3 -emit-module -module-name foo %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift >%t/out.txt
// RUN: %FileCheck %s <%t/out.txt
//
// Each batch should get 3 primaries; check that each has 3 modules _in the same numeric order_.
//
// CHECK: {{.*}}/swift {{.*}}-primary-file {{[^ ]*}}/file-[[A1:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[A2:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[A3:[0-9]+]].swift
// CHECK-SAME: -o {{.*}}/file-[[A1]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[A2]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[A3]]-{{[a-z0-9]+}}.swiftmodule
// CHECK: {{.*}}/swift {{.*}}-primary-file {{[^ ]*}}/file-[[B1:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[B2:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[B3:[0-9]+]].swift
// CHECK-SAME: -o {{.*}}/file-[[B1]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[B2]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[B3]]-{{[a-z0-9]+}}.swiftmodule
// CHECK: {{.*}}/swift {{.*}}-primary-file {{[^ ]*}}/file-[[C1:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[C2:[0-9]+]].swift {{.*}}-primary-file {{[^ ]*}}/file-[[C3:[0-9]+]].swift
// CHECK-SAME: -o {{.*}}/file-[[C1]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[C2]]-{{[a-z0-9]+}}.swiftmodule -o {{.*}}/file-[[C3]]-{{[a-z0-9]+}}.swiftmodule
