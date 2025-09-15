// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/InputClangModules)
// RUN: split-file %s %t

// - Fixup the input module file map
// RUN: sed -e "s|INPUTSDIR|%/t/InputClangModules|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|CUSTOMFRAMEWORKS|%S/Inputs/custom-frameworks|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// - Set up explicit dependencies for Client
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/InputClangModules/SwiftShims.pcm -Xcc -fswift-version-independent-apinotes

// - Build the APINotesFrameworkTest module using verison-independent APINotes
// RUN: %target-swift-emit-pcm -module-name APINotesFrameworkTest %S/Inputs/custom-frameworks/APINotesFrameworkTest.framework/Modules/module.modulemap -o %t/InputClangModules/APINotesFrameworkTest.pcm -Xcc -I -Xcc %S/Inputs/custom-frameworks/APINotesFrameworkTest.framework/Headers -Xcc -F -Xcc %S/Inputs/custom-frameworks -Xcc -fswift-version-independent-apinotes

// - Build the client
// RUN: %target-swift-frontend -typecheck -verify %t/client.swift -explicit-swift-module-map-file %t/map.json -disable-implicit-swift-modules -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -version-independent-apinotes

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
      "moduleName": "APINotesFrameworkTest",
      "isFramework": true,
      "clangModuleMapPath": "CUSTOMFRAMEWORKS/APINotesFrameworkTest.framework/Modules/module.modulemap",
      "clangModulePath": "INPUTSDIR/APINotesFrameworkTest.pcm"
  }  
]

//--- client.swift
import APINotesFrameworkTest
