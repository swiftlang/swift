// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s
// CHECK: {{"Frontend.NumInstructions"	[1-9][0-9]*$}}

public func foo() {
    print("hello")
}
