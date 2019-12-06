/// main <==> other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/mutual-with-swiftdeps/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift
// CHECK-FIRST: Disabling incremental build: could not read build record

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s
// CHECK-SECOND-NOT: Queuing

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s
// CHECK-THIRD: Queuing (initial): {compile: other.o <= other.swift}
// CHECK-THIRD: Queuing because of the initial set: {compile: main.o <= main.swift}
// CHECK-THIRD-NEXT: other.swift provides top-level name 'a'

