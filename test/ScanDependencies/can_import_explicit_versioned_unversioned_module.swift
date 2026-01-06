// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/module-cache)
// RUN: split-file %s %t

// - Fixup the input module file map
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|MYLIBMOD|%/t/inputs/MyLib.swiftmodule|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// - Set up explicit dependencies for MyExe
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/MyLib.swiftmodule %t/MyLib.swift -module-name MyLib -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -explicit-swift-module-map-file %t/map.json

// - Build MyExe
// RUN: %target-swift-frontend -c %t/MyExe.swift -I %t/inputs -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -explicit-swift-module-map-file %t/map.json -module-can-import MyLib -verify 

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
      "moduleName": "SwiftShims",
      "isFramework": false,
      "clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
      "clangModulePath": "INPUTSDIR/SwiftShims.pcm"
  },
  {
      "moduleName": "MyLib",
      "modulePath": "MYLIBMOD",
      "isFramework": false
  }  
]

//--- MyLib.swift
public func Foo() {
    print("foo")
}

//--- MyExe.swift
#if canImport(MyLib, _version: 42) // expected-warning {{cannot find user version number for module 'MyLib'; version number ignored}}
  // TODO(ParserValidation): expected-warning@-1 *{{cannot find user version number for module 'MyLib'; version number ignored}}
  #warning("versioned canImport() of unversioned module is working")
  // expected-warning@-1{{versioned canImport() of unversioned module is working}}
#else
  #warning("versioned canImport() of unversioned module is broken")
#endif
  
