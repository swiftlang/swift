// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation

// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1

// Now, do it again with range dependencies enabled:

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver  -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1


// Add an attribute to: a structure that no other file uses

// RUN: cp %t/fileB2.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver  -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output1 2>&1


// Add an attribute to: a structure that one other file uses

// RUN: cp %t/fileB3.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver  -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental -driver-dump-compiled-source-diffs >%t/output2  2>&1



// What if the user adds a close brace and new type in the middle?

// RUN: cp %t/fileB4.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver  -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output3 2>&1

// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift ./fileC.swift -module-name main -j1 -driver-show-incremental >& %t/output6
