// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/base/Bridging.h -scanner-output-dir %t -auto-bridging-header-chaining -scanner-debug-write-output \
// RUN:   %t/base/test.swift %t/base/foo.swift -I %t/include -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:Dummy > %t/dummy.cmd
// RUN: %swift_frontend_plain @%t/dummy.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/base/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: sed -e "s@VFS_DIR@%{/t:regex_replacement}/base@g" -e "s@EXTERNAL_DIR@%{/t:regex_replacement}/modified@g" %t/base.yaml > %t/overlay.yaml

// RUN: %target-swift-frontend %t/base/test.swift %t/base/foo.swift -O -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas @%t/MyApp.cmd -vfsoverlay %t/overlay.yaml -module-import-from-cas


//--- base/test.swift
import Dummy
public func testFunc() {
  non_existing_func()
}

//--- base/foo.swift
public func foo() {}

//--- modified/test.swift
import Dummy
public func testFunc() {
  dummy()
  bridge()
}

//--- base/Bridging.h
void bridge(void);

//--- include/module.modulemap
module Dummy {
 umbrella header "Dummy.h"
}

//--- include/Dummy.h
void dummy(void);

//--- base.yaml
{
  version: 0,
  roots: [
    {
      type: "directory-remap",
      name: "VFS_DIR",
      external-contents: "EXTERNAL_DIR"
    }
  ]
}
