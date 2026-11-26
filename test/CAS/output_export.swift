// Check outputs that are written on disk by exporting them from the CAS.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %{python} %t/gen.py > %t/test.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// The object file is large enough for the on-disk CAS to store it in its own
/// file, which can be cloned when it's exported.
// RUN: %target-swift-frontend-plain -c -cache-compile-job -Rcache-compile-job \
// RUN:   -cas-path %t/cas %t/test.swift -o %t/test.o -emit-module \
// RUN:   -emit-module-path %t/Test.swiftmodule @%t/MyApp.cmd 2>&1 \
// RUN:   | %FileCheck --check-prefix=CACHE-MISS %s
// RUN: cp %t/test.o %t/miss.o
// RUN: cp %t/Test.swiftmodule %t/miss.swiftmodule

/// Make the files in the CAS old. The outputs exported from them must still
/// look newly written, both when replaying and when computing them again.
// RUN: %{python} %t/age.py %t/cas
// RUN: rm %t/test.o %t/Test.swiftmodule
// RUN: %target-swift-frontend-plain -c -cache-compile-job -Rcache-compile-job \
// RUN:   -cas-path %t/cas %t/test.swift -o %t/test.o -emit-module \
// RUN:   -emit-module-path %t/Test.swiftmodule @%t/MyApp.cmd 2>&1 \
// RUN:   | %FileCheck --check-prefix=CACHE-HIT %s
// RUN: diff %t/miss.o %t/test.o
// RUN: diff %t/miss.swiftmodule %t/Test.swiftmodule
// RUN: %{python} %t/check_new.py %t/test.o %t/Test.swiftmodule

// RUN: rm %t/test.o %t/Test.swiftmodule
// RUN: %target-swift-frontend-plain -c -cache-compile-job -Rcache-compile-job \
// RUN:   -cache-disable-replay -cas-path %t/cas %t/test.swift -o %t/test.o \
// RUN:   -emit-module -emit-module-path %t/Test.swiftmodule @%t/MyApp.cmd
// RUN: diff %t/miss.o %t/test.o
// RUN: diff %t/miss.swiftmodule %t/Test.swiftmodule
// RUN: %{python} %t/check_new.py %t/test.o %t/Test.swiftmodule

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}test.o': id 'llvmcas://{{.*}}'
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}Test.swiftmodule': id 'llvmcas://{{.*}}'

//--- gen.py
for i in range(4000):
    print("public func f%d() {}" % i)

//--- age.py
import os
import sys

for root, _, files in os.walk(sys.argv[1]):
    for f in files:
        os.utime(os.path.join(root, f), (946684800, 946684800))

//--- check_new.py
import os
import sys

# Fail if any file has a modification time before 2001.
sys.exit(any(os.stat(f).st_mtime < 1e9 for f in sys.argv[1:]))
