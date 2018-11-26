// RUN: %{python} -c 'for i in range(500001): print "-DTEST_" + str(i)' > %t.resp
// RUN: %swiftc_driver -driver-print-jobs -module-name merge -emit-module %S/Inputs/main.swift %S/Inputs/lib.swift @%t.resp 2>&1 > %t.jobs.txt
// RUN: %FileCheck %s < %t.jobs.txt -check-prefix=MERGE

// MERGE: bin/swift{{c?}}
// MERGE: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp # -frontend -emit-module -primary-file {{[^ ]+}}/Inputs/main.swift {{[^ ]+}}/Inputs/lib.swift
// MERGE: -emit-module-doc-path [[PARTIAL_MODULE_A:[^ ]+]].swiftdoc
// MERGE: bin/swift{{c?}}
// MERGE: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp # -frontend -emit-module {{[^ ]+}}/Inputs/main.swift -primary-file {{[^ ]+}}/Inputs/lib.swift
// MERGE: -emit-module-doc-path [[PARTIAL_MODULE_B:[^ ]+]].swiftdoc
// MERGE: bin/swift{{c?}}
// MERGE: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp # -frontend -merge-modules -emit-module [[PARTIAL_MODULE_A]].swiftmodule [[PARTIAL_MODULE_B]].swiftmodule
