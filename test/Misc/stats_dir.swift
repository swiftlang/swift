// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -o %t/out.o -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -o %t/out.o -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv --exclude-timers %t
// RUN: %FileCheck -input-file %t/frontend.csv --implicit-check-not '{{time.swift}}' %s
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -o %t/out.o -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv --select-stat NumSourceLines --select-stat NumIRFunctions --select-stat NumLLVMBytesOutput %t
// RUN: %FileCheck -input-file %t/frontend.csv %s
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -wmo -num-threads 4 -o %t/out.o -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s
// RUN: echo '9000000000	"LLVM.NumLLVMBytesOutput"	1' >>%t/frontend.csv
// RUN: not %{python} %utils/process-stats-dir.py --compare-to-csv-baseline %t/frontend.csv %t

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -c -o %t/out.o -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/driver.csv %t
// RUN: %FileCheck -input-file %t/driver.csv %s
// RUN: %{python} %utils/process-stats-dir.py --compare-to-csv-baseline %t/driver.csv %t

// RUN: %target-swiftc_driver -c -o %t/out.o -stats-output-dir %t/this/is/not/a/directory %s 2>&1 | %FileCheck -check-prefix=CHECK-NODIR %s

// CHECK: {{"AST.NumSourceLines"	[1-9][0-9]*$}}
// CHECK: {{"IRModule.NumIRFunctions"	[1-9][0-9]*$}}
// CHECK: {{"LLVM.NumLLVMBytesOutput"	[1-9][0-9]*$}}

// CHECK-NODIR: {{Error opening -stats-output-dir file}}

public func foo() {
    print("hello")
}
