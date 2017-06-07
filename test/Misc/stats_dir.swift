// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -c -o %t/out.o -stats-output-dir %t %s
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s
// RUN: echo '9000000000	"LLVM.NumLLVMBytesOutput"	1' >>%t/frontend.csv
// RUN: not %utils/process-stats-dir.py --compare-to-csv-baseline %t/frontend.csv %t

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swiftc_driver -c -o %t/out.o -stats-output-dir %t %s
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/driver.csv %t
// RUN: %FileCheck -input-file %t/driver.csv %s
// RUN: %utils/process-stats-dir.py --compare-to-csv-baseline %t/driver.csv %t

// CHECK: LLVM.NumLLVMBytesOutput

public func foo() {
    print("hello")
}
