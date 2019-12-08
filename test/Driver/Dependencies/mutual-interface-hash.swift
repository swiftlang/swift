/// does-change <==> does-not-change

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/mutual-interface-hash/* %t
// RUN: touch -t 201401240005 %t/*

// Generate the build record...
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./does-change.swift ./does-not-change.swift -module-name main -j1 -v

// ...then reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/mutual-interface-hash/*.swiftdeps %t

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./does-change.swift ./does-not-change.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-CLEAN %s

// CHECK-CLEAN-NOT: Handled

// RUN: touch -t 201401240006 %t/does-change.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./does-change.swift ./does-not-change.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-CHANGE %s

// CHECK-CHANGE: Handled does-change.swift
// CHECK-CHANGE: Handled does-not-change.swift


// RUN: cp -r %S/Inputs/mutual-interface-hash/*.swiftdeps %t

// RUN: touch -t 201401240006 %t/does-not-change.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./does-change.swift ./does-not-change.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-NO-CHANGE %s

// CHECK-NO-CHANGE-NOT: Handled
// CHECK-NO-CHANGE: Handled does-not-change.swift
// CHECK-NO-CHANGE-NOT: Handled


// RUN: cp -r %S/Inputs/mutual-interface-hash/*.swiftdeps %t

// Make sure the files really were dependent on one another.

// RUN: touch -t 201401240007 %t/does-not-change.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./does-change.swift ./does-not-change.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-REBUILD-DEPENDENTS %s

// CHECK-REBUILD-DEPENDENTS-DAG: Handled does-not-change.swift
// CHECK-REBUILD-DEPENDENTS-DAG: Handled does-change.swift


// Check that cascading builds triggered by the build record are still
// considered cascading.

// RUN: cp -r %S/Inputs/mutual-interface-hash/*.swiftdeps %t
// RUN: sed -E -e 's/"[^"]*does-not-change.swift":/& !dirty/' -i.prev %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./does-change.swift ./does-not-change.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-REBUILD-DEPENDENTS %s
