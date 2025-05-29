// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: sed -e "s@VFS_DIR@%{/t:regex_replacement}/vfs@g" -e "s@EXTERNAL_DIR@%{/t:regex_replacement}/hidden@g" %t/base.yaml > %t/overlay.yaml

// RUN: %target-swift-frontend -emit-module -module-name B -o %t/B.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/hidden/B.swiftinterface -enable-library-evolution %t/hidden/B.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/vfs/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -vfsoverlay %t/overlay.yaml -Xcc -ivfsoverlay -Xcc %t/overlay.yaml -I %t/vfs

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas -vfsoverlay %t/overlay.yaml \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/vfs/test.swift @%t/MyApp.cmd

//--- hidden/test.swift
import A
import B

//--- hidden/module.modulemap
module A {
  header "A.h"
  export *
}

//--- hidden/A.h
void a(void);

//--- hidden/B.swift
public func b() {}

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
