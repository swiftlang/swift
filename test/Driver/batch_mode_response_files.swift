// Batch jobs go through a different code path than other jobs, so make sure
// that they also use response files correctly when their argument lists are
// too long.

// RUN: %{python} -c 'for i in range(500001): print("-DTEST_" + str(i))' > %t.resp
// RUN: %swiftc_driver -driver-print-jobs -module-name batch -enable-batch-mode -j 1 -c %S/Inputs/main.swift %S/Inputs/lib.swift @%t.resp 2>&1 > %t.jobs.txt
// RUN: %FileCheck %s < %t.jobs.txt -check-prefix=BATCH

// BATCH: bin{{/|\\\\}}swift{{c?}}
// BATCH: @{{[^ ]*}}arguments-{{[0-9a-zA-Z]+}}.resp{{"?}} # -frontend -c -primary-file {{[^ ]+}}/Inputs/main.swift{{"?}} -primary-file {{[^ ]+}}/Inputs/lib.swift
