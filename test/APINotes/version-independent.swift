// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Swift)
// RUN: %empty-directory(%t/CFrameworks)
// RUN: %empty-directory(%t/CFrameworks/Bar.framework)
// RUN: %empty-directory(%t/CFrameworks/Bar.framework/Headers)
// RUN: %empty-directory(%t/CFrameworks/Bar.framework/Modules)
// RUN: split-file %s %t

// 0. Fixup the input module file map
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/input_map.json.template > %t/input_map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/input_map.json.template1 > %t/input_map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/input_map.json.template2 > %t/input_map.json.template3
// RUN: sed -e "s|BARMOD|%t/inputs/Bar.pcm|g" %t/input_map.json.template3 > %t/input_map.json.template4
// RUN: sed -e "s|BARMAP|%t/CFrameworks/Bar.framework/Modules/module.modulemap|g" %t/input_map.json.template4 > %t/input_map.json.template5
// RUN: sed -e "s|FOOMOD|%/t/inputs/Foo.swiftmodule|g" %t/input_map.json.template5 > %t/input_map.json.template6
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/input_map.json.template6 > %t/input_map.json

// 1. Build `Foo.swiftmodule` and `Foo.swiftinterface` using `-swift-version 5`
// RUN: %target-swift-frontend -emit-module -F%t/CFrameworks %t/Foo.swift -emit-module-path %t/Swift/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/Swift/Foo.swiftmodule/%target-swiftinterface-name -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -module-cache-path %t/ModuleCache -enable-library-evolution -swift-version 5 -version-independent-apinotes

// 2. Prepare for an explicit build of `Foo` and `main` by pre-compiling its dependencies
// RUN: %target-swift-emit-pcm -module-name Bar -o %t/inputs/Bar.pcm %t/CFrameworks/Bar.framework/Modules/module.modulemap -Xcc -fswift-version-independent-apinotes
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm -Xcc -fswift-version-independent-apinotes

// 3. Build textual `Foo.swiftinterface` into a binary module using its interface-coded `-swift-version 5`
// RUN: rm %t/Swift/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -compile-module-from-interface %t/Swift/Foo.swiftmodule/%target-swiftinterface-name -o %t/inputs/Foo.swiftmodule -module-name Foo -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -Xcc -fno-implicit-modules -Xcc -fno-implicit-module-maps -explicit-swift-module-map-file %t/input_map.json -swift-version 5 -version-independent-apinotes

// 4. Build `main.swift` using `-swift-version 4`
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Main.swiftmodule -disable-implicit-swift-modules -module-cache-path %t.module-cache -explicit-swift-module-map-file %t/input_map.json -swift-version 4 -version-independent-apinotes %t/main.swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import

// Because the `Bar` Clang module gets built with `-fswift-version-independent-apinotes`, it encodes all versioned APINotes into the resulting PCM. This means that `Foo.swiftinterface`, which gets built with Swift version 5, is able to resolve references to `funcBro` in its inlinable code to Bar's `funcBar`, according to version 5 APINotes, and `main.swift` is able to resolve a reference to `funkBus` to Bar's `funcBaz`, according to version 4 APINotes. `main.swift` compilation also gets the APINotes that ensures that `barGlobal`'s type is `unsigned` rather than `int`, as it appears in the source, as per APINotes version 4.

//--- CFrameworks/Bar.framework/Headers/Bar.h
int barGlobal = 42;
int funcBar(void) {
  return 1;
}
int funcBaz(void) {
  return 2;
}
int funcNuc(void) {
  return 3;
}

//--- CFrameworks/Bar.framework/Headers/Bar.apinotes
---
Name: Bar
SwiftVersions:
- Version: 4.0
  Functions:
  - Name: funcBar
    SwiftName: "funcBoom()"
  - Name: funcBaz
    SwiftName: "funkBus()"

  Globals:
  - Name: barGlobal
    Type: "unsigned"

- Version: 5.0
  Functions:
  - Name: funcBar
    SwiftName: "funcBro()"
  - Name: funcNuc
    SwiftName: "funcNot()"

Functions:
  - Name: funcBar
    SwiftName: "funcBroom()"

//--- CFrameworks/Bar.framework/Modules/module.modulemap
framework module Bar [system] {
  header "Bar.h"
  export *
}

//--- Foo.swift
import Bar
@inlinable
public func foo() -> Int {
    return Int(funcBro())
}

//--- main.swift
import Foo
import Bar

let x: UInt32 = barGlobal
print(41 + foo() + Int(funkBus()) + Int(funcNot()))

//--- input_map.json.template
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
      "moduleName": "Bar",
      "isFramework": true,
      "clangModuleMapPath": "BARMAP",
      "clangModulePath": "BARMOD"
  },
  {
      "moduleName": "Foo",
      "modulePath": "FOOMOD",
      "isFramework": true
  },
  {
      "moduleName": "SwiftShims",
      "isFramework": false,
      "clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
      "clangModulePath": "INPUTSDIR/SwiftShims.pcm"
  }
]
