// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -O -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/Bridging.h -scanner-output-dir %t -auto-bridging-header-chaining -scanner-debug-write-output \
// RUN:   %t/test.swift %t/foo.swift -I %t/include -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd -b %t/header.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend-plain @%t/header.cmd %t/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain @%t/header.cmd %t/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json

// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend-plain %t/test.swift %t/foo.swift -O -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cache-compile-job -cas-path %t/cas @%t/MyApp.cmd -gen-reproducer -gen-reproducer-dir %t/crash

// RUN: %FileCheck %s --input-file=%t/crash/reproduce.sh
// CHECK-NOT: -gen-reproducer

/// The inputs are captured as regular files inside the reproducer and all the
/// paths owned by the reproducer are relative to the reproducer directory.
// RUN: %FileCheck %s --check-prefix=RELATIVE --input-file=%t/crash/reproduce.sh
// RELATIVE: "inputs{{.*}}test.swift" "inputs{{.*}}foo.swift"
// RELATIVE: "-emit-module-path" "outputs{{.*}}Test.swiftmodule"
// RELATIVE: "-o" "outputs{{.*}}test.o"
// RELATIVE: "-cas-path" "cas"
// RELATIVE: "-cas-fs-input-overlay" "-module-import-from-cas"

/// Delete all inputs from the original compilation and run the reproducer.
// RUN: rm -rf %t/include %t/test.swift %t/foo.swift
// RUN: cd %t/crash && %swift_frontend_plain @reproduce.sh

/// The reproducer writes its outputs into its own directory. Note `%:t` is the
/// layout the reproducer mirrors: the path of `%t` with the root turned into a
/// regular directory, e.g. `C:/dir` becomes `C/dir`.
// RUN: ls %t/crash/outputs/%:t/test.o

/// The captured inputs are editable and take precedence over the CAS content.
// RUN: echo "public func added() { undefined_function() }" >> %t/crash/inputs/%:t/foo.swift
// RUN: cd %t/crash && not %swift_frontend_plain @reproduce.sh 2>&1 | %FileCheck %s --check-prefix=EDITED
// EDITED: error: cannot find 'undefined_function' in scope

/// Also test module jobs.
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:Dummy > %t/dummy.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Simple > %t/simple.cmd
// RUN: %swift_frontend_plain @%t/dummy.cmd -gen-reproducer -gen-reproducer-dir %t/crash-2
// RUN: %FileCheck %s --input-file=%t/crash-2/reproduce.sh
// RUN: cd %t/crash-2 && %swift_frontend_plain @reproduce.sh
// RUN: %swift_frontend_plain @%t/simple.cmd -gen-reproducer -gen-reproducer-dir %t/crash-3
// RUN: %FileCheck %s --input-file=%t/crash-3/reproduce.sh
// RUN: cd %t/crash-3 && %swift_frontend_plain @reproduce.sh

/// Test the file list and output file map inputs. Recreate the sources that
/// were deleted above. The paths that are matched back against the input names,
/// i.e. the file list entries and the output file map keys, all have to be
/// spelled the same way, hence `%/t` throughout.
// RUN: split-file %s %t
// RUN: echo "%/t/test.swift" > %t/inputs.txt
// RUN: echo "%/t/foo.swift" >> %t/inputs.txt
// RUN: echo "%/t/test.swift" > %t/primary.txt
// RUN: echo "%/t/test.o" > %t/output.txt
// RUN: echo "%/t/test.o" > %t/index-unit.txt
// RUN: sed -e "s@TMP_DIR@%{/t:regex_replacement}@g" %t/supp.map > %t/supp.json

// RUN: %target-swift-frontend-plain -filelist %t/inputs.txt -primary-filelist %t/primary.txt \
// RUN:   -output-filelist %t/output.txt -index-unit-output-path-filelist %t/index-unit.txt \
// RUN:   -supplementary-output-file-map %t/supp.json -O -emit-module -c -module-name Test \
// RUN:   -cache-compile-job -cas-path %t/cas @%t/MyApp.cmd -gen-reproducer -gen-reproducer-dir %t/crash-4

/// Every file list is captured in the reproducer and points back into it.
// RUN: %FileCheck %s --check-prefix=FILELIST --input-file=%t/crash-4/reproduce.sh
// FILELIST-DAG: "-filelist" "inputs{{.*}}inputs.txt"
// FILELIST-DAG: "-primary-filelist" "inputs{{.*}}primary.txt"
// FILELIST-DAG: "-output-filelist" "inputs{{.*}}output.txt"
// FILELIST-DAG: "-index-unit-output-path-filelist" "inputs{{.*}}index-unit.txt"
// FILELIST-DAG: "-supplementary-output-file-map" "inputs{{.*}}supp.json"

// RUN: %FileCheck %s --check-prefix=INPUT-LIST --input-file=%t/crash-4/inputs/%:t/inputs.txt
// INPUT-LIST: inputs{{.*}}test.swift
// INPUT-LIST: inputs{{.*}}foo.swift

// RUN: %FileCheck %s --check-prefix=OUTPUT-LIST --input-file=%t/crash-4/inputs/%:t/output.txt
// OUTPUT-LIST: outputs{{.*}}test.o

/// The index unit output paths only name the outputs in the index data, so they
/// are copied over unchanged.
// RUN: %FileCheck %s --check-prefix=INDEX-LIST --input-file=%t/crash-4/inputs/%:t/index-unit.txt
// INDEX-LIST-NOT: {{^}}outputs
// INDEX-LIST: {{^}}{{.*}}test.o

/// The output file map is rewritten to use the captured inputs and to write the
/// supplementary outputs inside the reproducer.
// RUN: %FileCheck %s --check-prefix=SUPP --input-file=%t/crash-4/inputs/%:t/supp.json
// SUPP: "inputs{{.*}}test.swift"
// SUPP-DAG: swiftmodule: "outputs{{.*}}Test.swiftmodule"
// SUPP-DAG: swiftdoc: "outputs{{.*}}Test.swiftdoc"

/// Delete the original inputs and run the reproducer.
// RUN: rm -rf %t/include %t/test.swift %t/foo.swift %t/inputs.txt %t/primary.txt %t/output.txt %t/index-unit.txt %t/supp.json
// RUN: cd %t/crash-4 && %swift_frontend_plain @reproduce.sh
// RUN: ls %t/crash-4/outputs/%:t/Test.swiftmodule

/// Caching and the input overlay are mutually exclusive.
// RUN: not %target-swift-frontend-plain -typecheck %t/test.swift -module-name Test \
// RUN:   -cache-compile-job -cas-path %t/cas -cas-fs-input-overlay 2>&1 | %FileCheck %s --check-prefix=CONFLICT
// CONFLICT: error: argument '-cas-fs-input-overlay' is not allowed with '-cache-compile-job'

//--- test.swift
import Dummy
import Simple
public func testFunc() {
  foo()
  bridge()
  simple()
}

//--- foo.swift
public func foo() {}

//--- Bridging.h
void bridge(void);

//--- supp.map
"TMP_DIR/test.swift":
  swiftmodule: "TMP_DIR/Test.swiftmodule"
  swiftdoc: "TMP_DIR/Test.swiftdoc"

//--- include/module.modulemap
module Dummy {
 umbrella header "Dummy.h"
}

//--- include/Dummy.h
void dummy(void);

//--- include/Simple.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Simple -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
import Swift
import Dummy
public func simple() { }
