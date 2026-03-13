// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/inputs

// RUN: split-file %s %t
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|CHEADERSDIR|%/S/Inputs/CHeaders|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/A.swiftmodule -emit-module-doc-path %t/inputs/A.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/A.swiftsourceinfo -import-underlying-module -I%S/Inputs/CHeaders -module-cache-path %t.module-cache %t/A.swift -module-name A -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %target-swift-emit-pcm -module-name A -o %t/inputs/A.pcm %S/Inputs/CHeaders/module.modulemap
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm

// RUN: %target-swift-frontend -emit-module-path %t/Foo.swiftmodule -disable-implicit-swift-modules -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-cache-path %t.module-cache -explicit-swift-module-map-file %t/map.json -primary-file %t/test.swift -emit-dependencies-path %t/test.d -emit-reference-dependencies-path %t/test.swiftdeps -o %t/test.o

// Verify that make-style dependencies contain header files of transitive clang module dependency
// RUN: cat %t/test.d | %FileCheck %s -check-prefix=MAKEDEP-CHECK
// MAKEDEP-CHECK: {{.*}}test{{/|\\}}ScanDependencies{{/|\\}}Inputs{{/|\\}}CHeaders{{/|\\}}A.h

// Verify that Swift reference dependencies contain header files of transitive clang module dependency
// RUN: llvm-bcanalyzer --dump %t/test.swiftdeps | %FileCheck %s -check-prefix=SWIFTDEPS-CHECK
// SWIFTDEPS-CHECK: <IDENTIFIER_NODE abbrevid=8/> blob data = '{{.*}}test{{/|\\}}ScanDependencies{{/|\\}}Inputs{{/|\\}}CHeaders{{/|\\}}A.h'


//--- A.swift
public func anotherFuncA() {}

//--- map.json.template
[
  {
      "moduleName": "A",
      "modulePath": "INPUTSDIR/A.swiftmodule",
      "docPath": "INPUTSDIR/A.swiftdoc",
      "sourceInfoPath": "INPUTSDIR/A.swiftsourceinfo",
      "isFramework": false,
  },
  {
      "moduleName": "A",
      "clangModulePath": "INPUTSDIR/A.pcm",
      "clangModuleMapPath": "CHEADERSDIR/module.modulemap"
  },
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
      "moduleName": "SwiftShims",
      "isFramework": false,
      "clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
      "clangModulePath": "INPUTSDIR/SwiftShims.pcm"
}]

//--- test.swift
import A

func callA() {
  funcA()
  anotherFuncA()
}
