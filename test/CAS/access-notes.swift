// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -access-notes-path %t/extra.accessnotes

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd -access-notes-path %t/extra.accessnotes -Raccess-note=all 2>&1 | %FileCheck %s

//--- main.swift
class Extant {
  func good(_: Int) {} // expected-remark * {{}} expected-note * {{}}
  // CHECK-DAG: main.swift:2:{{[0-9]+}}: remark: implicitly added '@objc' to this instance method, as specified by access note for Access notes containing future, unknown syntax
  // CHECK-DAG: note: add '@objc' explicitly to silence this warning

  func bad(_: Int?) {} // expected-remark * {{}}
  // CHECK-DAG: main.swift:6:{{[0-9]+}}: remark: ignored access note: instance method cannot be marked '@objc' by an access note because the type of the parameter cannot be represented in Objective-C; did not implicitly add '@objc' to this instance method, even though it was specified by access note for Access notes containing future, unknown syntax
}

//--- extra.accessnotes
Reason: Access notes containing future, unknown syntax
Notes:
- Name: 'Extant.good(_:)'
  ObjC: true
- Name: 'Extant.bad(_:)'
  ObjC: true
