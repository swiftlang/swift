// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/a.swift %t/b.swift %t/c.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: echo "" >> %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map2.casid
// RUN: echo "missing" > %t/missing.json
// RUN: llvm-cas --cas %t/other-cas --make-blob --data %t/missing.json > %t/missing.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd

/// The jobs in the same batch build share all the references in the base key.
/// Only the primary inputs, stored in the base key node itself, differ.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o > %t/job1.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   %t/a.swift -primary-file %t/b.swift -primary-file %t/c.swift -o %t/b.o -o %t/c.o > %t/job2.casid
// RUN: not diff %t/job1.casid %t/job2.casid
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/job1.casid > %t/job1.refs
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/job2.casid > %t/job2.refs
// RUN: diff %t/job1.refs %t/job2.refs

/// Same for the jobs using file lists.
// RUN: echo "%t/a.swift" > %t/filelist
// RUN: echo "%t/b.swift" >> %t/filelist
// RUN: echo "%t/c.swift" >> %t/filelist
// RUN: echo "%t/a.swift" > %t/primary-filelist-1
// RUN: echo "%t/b.swift" > %t/primary-filelist-2
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -filelist %t/filelist -primary-filelist %t/primary-filelist-1 > %t/filelist1.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -filelist %t/filelist -primary-filelist %t/primary-filelist-2 > %t/filelist2.casid
// RUN: not diff %t/filelist1.casid %t/filelist2.casid
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/filelist1.casid > %t/filelist1.refs
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/filelist2.casid > %t/filelist2.refs
// RUN: sed '$d' %t/filelist1.refs > %t/filelist1.shared.refs
// RUN: sed '$d' %t/filelist2.refs > %t/filelist2.shared.refs
// RUN: diff %t/filelist1.shared.refs %t/filelist2.shared.refs

/// The order of the inputs affects the key.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -primary-file %t/b.swift %t/a.swift %t/c.swift -o %t/b.o > %t/order1.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   %t/a.swift -primary-file %t/b.swift %t/c.swift -o %t/b.o > %t/order2.casid
// RUN: not diff %t/order1.casid %t/order2.casid

/// Changing the CAS ID only changes the referenced CAS ID, not the arguments.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map2.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o > %t/casid.casid
// RUN: not diff %t/job1.casid %t/casid.casid
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/casid.casid > %t/casid.refs
// RUN: not diff %t/job1.refs %t/casid.refs
// RUN: head -n 3 %t/job1.refs > %t/job1.shared.refs
// RUN: head -n 3 %t/casid.refs > %t/casid.shared.refs
// RUN: diff %t/job1.shared.refs %t/casid.shared.refs

/// Changing the clang arguments only changes the clang arguments.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o -Xcc -DFOO > %t/xcc.casid
// RUN: llvm-cas --cas %t/cas --ls-node-refs @%t/xcc.casid > %t/xcc.refs
// RUN: sed -n 2p %t/job1.refs > %t/job1.cmd.ref
// RUN: sed -n 2p %t/xcc.refs > %t/xcc.cmd.ref
// RUN: diff %t/job1.cmd.ref %t/xcc.cmd.ref
// RUN: sed -n 3p %t/job1.refs > %t/job1.xcc.ref
// RUN: sed -n 3p %t/xcc.refs > %t/xcc.xcc.ref
// RUN: not diff %t/job1.xcc.ref %t/xcc.xcc.ref

/// A labeled option with a value that is not a CAS ID is allowed.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o -debug-module-path %t/Test.swiftmodule

/// A CAS ID that is not in the CAS is an error.
// RUN: not %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/missing.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o 2>&1 | %FileCheck %s --check-prefix=MISSING
// RUN: not %target-swift-frontend-plain -cache-compile-job -cas-path %t/cas -c @%t/MyApp.cmd @%t/missing.casid \
// RUN:   -primary-file %t/a.swift %t/b.swift %t/c.swift -o %t/a.o 2>&1 | %FileCheck %s --check-prefix=MISSING
// MISSING: CAS ID 'llvmcas://{{.*}}' for '-explicit-swift-module-map-file' is not found

/// Print the cache key.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job -c @%t/MyApp.cmd @%t/map.casid \
// RUN:   %t/a.swift -primary-file %t/b.swift %t/c.swift -o %t/b.o > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json %t/b.swift > %t/key
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-compile-cache-key @%t/key | %FileCheck %s --check-prefix=PRINT

// PRINT: Cache Key llvmcas://
// PRINT-NEXT: Swift Compiler Invocation Info:
// PRINT-NEXT:   command-line
// PRINT:          -c
// PRINT-NOT:      -clang-include-tree-filelist
// PRINT:          -explicit-swift-module-map-file llvmcas://
// PRINT-NEXT:     <inputs> llvmcas://
// PRINT-NEXT:       {{.*}}a.swift
// PRINT-NEXT:       {{.*}}b.swift
// PRINT-NEXT:       {{.*}}c.swift
// PRINT:        clang-arguments
// PRINT:        job-arguments
// PRINT-NEXT:     -primary-file {{.*}}b.swift
// PRINT-NEXT:   include-tree
// PRINT-NEXT:     -clang-include-tree-filelist llvmcas://
// PRINT-NEXT:   version
// PRINT-NEXT:     {{.*}}Swift version
// PRINT-NEXT: Input index: 1

//--- a.swift
func a() {}

//--- b.swift
func b() {}

//--- c.swift
func c() {}
