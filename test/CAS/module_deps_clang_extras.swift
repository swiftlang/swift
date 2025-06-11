// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/cas
// RUN: split-file %s %t
// RUN: sed "s|DIR|%/t|g" %t/hmap.json.template > %t/hmap.json
// RUN: sed "s|DIR|%/t|g" %t/test.yaml.template > %t/test.yaml
// RUN: %hmaptool write %t/hmap.json %t/test.hmap

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache \
// RUN:   %t/Test.swift -module-name Test -o %t/deps.json -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/include/module.modulemap -Xcc -ivfsoverlay -Xcc %t/test.yaml \
// RUN:   -Xcc -I%t/test.hmap -module-load-mode prefer-serialized -scanner-output-dir %t \
// RUN:   -import-objc-header %t/Bridge.h -auto-bridging-header-chaining
// RUN: %validate-json %t/deps.json &>/dev/null

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test casFSRootID > %t/fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/fs.casid | %FileCheck %s -DDIR=%basename_t -check-prefix FS_ROOT

// FS_ROOT: [[DIR]].tmp/hidden/Dummy.h
// FS_ROOT: [[DIR]].tmp/hidden/a.h
// FS_ROOT: [[DIR]].tmp/hidden/b.h

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:Dummy > %t/dummy.cmd
// RUN: %swift_frontend_plain @%t/dummy.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/objc.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/objc.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/objc.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd %t/Test.swift \
// RUN:   -emit-module -o %t/test.swiftmodule

//--- Test.swift
import Dummy
func test() {}

//--- Bridge.h
#include "b.h"

//--- hidden/module.modulemap
module Dummy {
 umbrella header "Dummy.h"
}

//--- hidden/Dummy.h
#include "a.h"
void dummy(void);

//--- hidden/a.h
/* empty file */

//--- hidden/b.h
/* empty file */

//--- hmap.json.template
{
  "mappings": {
    "a.h": "DIR/hidden/a.h",
    "b.h": "DIR/hidden/b.h"
  }
}

//--- test.yaml.template
{
  "version": 0,
  "case-sensitive": "false",
  "use-external-names": true,
  "roots": [
    {
      "type": "file",
      "name": "DIR/include/module.modulemap",
      "external-contents": "DIR/hidden/module.modulemap"
    },
    {
      "type": "file",
      "name": "DIR/include/Dummy.h",
      "external-contents": "DIR/hidden/Dummy.h"
    },
  ]
}

