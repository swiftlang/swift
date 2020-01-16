/// crash ==> main | crash --> other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple-with-swiftdeps/* %t
// RUN: touch -t 201401240005 %t/*

// Initially compile all inputs, crash will fail.

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s
// CHECK-NOT: warning
// CHECK: Handled main.swift
// CHECK: Handled crash.swift
// CHECK-NOT: Handled other.swift

// Put crash.swift first to assure it gets scheduled first.
// The others get queued, but not dispatched because crash crashes.

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./crash.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-BAD-ONLY %s

// CHECK-BAD-ONLY-NOT: warning
// CHECK-BAD-ONLY-NOT: Handled
// CHECK-BAD-ONLY: Handled crash.swift
// CHECK-BAD-ONLY-NOT: Handled

// Make crash succeed and all get compiled, exactly once.

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-OKAY %s
// CHECK-OKAY: Handled main.swift
// CHECK-OKAY: Handled crash.swift
// CHECK-OKAY: Handled other.swift
// CHECK-OKAY-NOT: Handled

// Make crash crash again:

// RUN: touch -t 201401240006 %t/crash.swift
// RUN: rm %t/crash.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s

// And repair crash:

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-OKAY-2 %s

// CHECK-OKAY-2-DAG: Handled crash.swift
// CHECK-OKAY-2-DAG: Handled other.swift
// CHECK-OKAY-2-DAG: Handled main.swift

// Touch main so its newer, remove main.swiftdeps and make crash crash:
// Driver will fall back to non-incremental, will compile main,
// will compile crash, and then stop.

// RUN: touch -t 201401240006 %t/main.swift
// RUN: rm %t/main.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-NO-MAIN-SWIFTDEPS %s

// CHECK-NO-MAIN-SWIFTDEPS-NOT: warning
// CHECK-NO-MAIN-SWIFTDEPS: Handled main.swift
// CHECK-NO-MAIN-SWIFTDEPS: Handled crash.swift
// CHECK-NO-MAIN-SWIFTDEPS-NOT: Handled other.swift


// Touch all files earlier than last compiled date in the build record.

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 -driver-show-incremental |tee /tmp/out1 | %FileCheck -check-prefix=CHECK-CURRENT-WITH-CRASH %s

// CHECK-CURRENT-WITH-CRASH: Handled main.swift
// CHECK-CURRENT-WITH-CRASH: Handled crash.swift
// CHECK-CURRENT-WITH-CRASH: Handled other.swift
// CHECK-CURRENT-WITH-CRASH-NOT: Handled

// Touch other, but remove its swiftdeps. Should compile everything.

// RUN: touch -t 201401240006 %t/other.swift
// RUN: rm %t/other.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./crash.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s
