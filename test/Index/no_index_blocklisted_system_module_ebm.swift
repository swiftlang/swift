// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModCache)
// RUN: %empty-directory(%t/ModInputs)
// RUN: %empty-directory(%t/mock.sdk/SystemFrameworks/Foo.framework/modules)
// RUN: split-file %s %t

// - Fixup the input module file map
// RUN: sed -e "s|INPUTSDIR|%/t/ModInputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|FOOMOD|%t/mock.sdk/SystemFrameworks/Foo.framework/modules/Foo.swiftmodule/%target-swiftmodule-name|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// - Set up explicit dependencies for Foo
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/ModInputs/SwiftShims.pcm -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules

// - Build Foo module dependency
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/mock.sdk/SystemFrameworks/Foo.framework/modules/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/mock.sdk/SystemFrameworks/Foo.framework/modules/Foo.swiftmodule/%target-swiftinterface-name -module-cache-path %t/ModCache %t/Foo.swift -module-name Foo -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules  -explicit-swift-module-map-file %t/map.json

// - Build with indexing the main test module importing Foo, ensure system module Foo gets indexed
// RUN: %target-swift-frontend -typecheck -swift-version 5 -index-system-modules -index-store-path %t/idx1 -index-ignore-stdlib -module-cache-path %t/ModCache -Rindexing-system-module %t/Client.swift -disable-deserialization-recovery -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -Fsystem %t/SystemFrameworks/ -sdk '' -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules -explicit-swift-module-map-file %t/map.json &> %t.indexed.out
// RUN: %FileCheck %s -input-file=%t.indexed.out --check-prefix=CHECK-INDEXED

// - Build with indexing the main test module importing Foo, which is blocklisted and ensure Foo does not get indexed because it is blocklisted
// RUN: %target-swift-frontend -typecheck -swift-version 5 -index-system-modules -index-store-path %t/idx2 -index-ignore-stdlib -module-cache-path %t/ModCache -Rindexing-system-module %t/Client.swift -disable-deserialization-recovery -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -Fsystem %t/SystemFrameworks/ -sdk '' -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules -explicit-swift-module-map-file %t/map.json -blocklist-file %t/blocklist.yml &> %t.blocklisted.out
// RUN: %FileCheck %s -input-file=%t.blocklisted.out --check-prefix=CHECK-BLOCKLISTED

// CHECK-INDEXED-NOT: skipping because of a broken swiftinterface
// CHECK-BLOCKLISTED: remark: indexing system module at {{.*}}Foo.swiftmodule{{/|\\}}{{.*}}.swiftmodule; skipping because of a broken swiftinterface

//--- blocklist.yml
---
ShouldUseBinaryModule:
  ModuleName:
    - Foo # for tests

//--- map.json.template
[
  {
      "moduleName": "Swift",
      "modulePath": "STDLIBMOD",
      "isFramework": false
  },
  {
      "moduleName": "SwiftOnoneSupport",
      "modulePath": "ONONEMOD",
      "isFramework": false
  },
  {
      "moduleName": "Foo",
      "modulePath": "FOOMOD",
      "isFramework": false,
      "isSystem": true
  },  
  {
      "moduleName": "SwiftShims",
      "isFramework": false,
      "clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
      "clangModulePath": "INPUTSDIR/SwiftShims.pcm"
}]

//--- Foo.swift
struct Foo {}

//--- Client.swift
import Foo
