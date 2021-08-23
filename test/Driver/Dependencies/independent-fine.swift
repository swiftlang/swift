// main | other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/independent-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// RUN: ls %t/main~buildrecord.swiftdeps

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// Don't change the priors mod time.
// RUN: touch -t 201401240006 %t/*.swift
// RUN: touch -t 201401240006 %t/*.swiftdeps
// RUN: touch -t 201401240006 %t/*.json
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// RUN: touch -t 201401240007 %t/main.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/independent-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST-MULTI %s

// CHECK-FIRST-MULTI: Handled main.swift
// CHECK-FIRST-MULTI: Handled other.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// Don't change the priors mod time.
// RUN: touch -t 201401240006 %t/*.swift
// RUN: touch -t 201401240006 %t/*.swiftdeps
// RUN: touch -t 201401240006 %t/*.json
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST-MULTI %s


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/independent-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-SINGLE %s
// CHECK-SINGLE: Handled main.swift

// RUN: ls %t/main~buildrecord.swiftdeps
