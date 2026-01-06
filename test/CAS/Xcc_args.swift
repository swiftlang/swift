// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -Xcc -D_VERSION=1 \
// RUN:   -Xcc -fmodule-map-file=%t/include/module.modulemap -Xcc -ivfsoverlay -Xcc %t/empty.yaml \
// RUN:   -Xcc -I%t/empty.hmap -module-load-mode prefer-serialized \
// RUN:   -file-compilation-dir %t

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %FileCheck %s --input-file=%t/MyApp.cmd

// CHECK: "-direct-clang-cc1-module-build"
// CHECK-NOT: "-fdebug-compilation-dir={{.*}}"
// CHECK: "_VERSION=1"
// CHECK-NOT: hmap.json
// CHECK-NOT: -ivfsoverlay
// CHECK-NOT: -fmodule-map-file

// RUN: %target-swift-frontend-plain \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift @%t/MyApp.cmd

//--- test.swift
private import _Macro

public func test() {
  // Check the VERSION is from command-line, thus a Int32, not string.
  let _ : Int32 = VERSION
}

//--- include/module.modulemap
module _Macro {
  umbrella "."
  export *
}

//--- include/Macro.h
#if defined(_VERSION)
#define VERSION _VERSION
#else
#define VERSION "not available"
#endif

//--- hmap.json
{
  "mappings": {}
}

//--- empty.yaml
{
  "version": 0,
  "case-sensitive": "false",
  "use-external-names": true,
  "roots": []
}
