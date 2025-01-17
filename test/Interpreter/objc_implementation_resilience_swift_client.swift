// Variant of Interpreter/objc_implementation_swift_client.swift that tests resilient stored properties.
// Will not execute correctly without ObjC runtime support.
// REQUIRES: rdar109171643

// Test doesn't pass on all platforms (rdar://101543397)
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t/frameworks/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t/frameworks/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t/frameworks/objc_implementation.framework/objc_implementation) -D RESILIENCE -enable-experimental-feature CImplementation -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple -emit-module-path %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t/frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t/frameworks/objc_implementation.framework/objc_implementation %S/objc_implementation.swift

//
// Execute this file
//
// RUN: %empty-directory(%t/swiftmod)
// RUN: %target-build-swift %S/objc_implementation_swift_client.swift -module-cache-path %t/swiftmod/mcp -F %t/frameworks -o %t/swiftmod/a.out -module-name main -D RESILIENCE -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple
// RUN: %target-codesign %t/swiftmod/a.out
// RUN: %target-run %t/swiftmod/a.out | %FileCheck %S/objc_implementation_swift_client.swift -check-prefixes CHECK,CHECK-RESILIENCE

//
// Execute again, without the swiftmodule this time
//
// RUN: mv %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule.disabled
// RUN: %empty-directory(%t/clangmod)
// RUN: %target-build-swift %S/objc_implementation_swift_client.swift -module-cache-path %t/clangmod/mcp -F %t/frameworks -o %t/clangmod/a.out -module-name main -D RESILIENCE -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple
// RUN: %target-codesign %t/clangmod/a.out
// RUN: %target-run %t/clangmod/a.out | %FileCheck %S/objc_implementation_swift_client.swift --check-prefixes CHECK,CHECK-RESILIENCE

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_ObjCImplementationWithResilientStorage

@main struct Main {
  static func main() {
    ImplClass.runTests()
    
    do {
      print("*** SwiftClientSubclass init ***")
      let swiftClientSub = SwiftClientSubclass()
      swiftClientSub.testSelf()
      print("*** SwiftClientSubclass end ***")
    }
  }
}
