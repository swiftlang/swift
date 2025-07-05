// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)

// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/A.framework/Modules/A.swiftmodule)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/A.framework/Headers)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/CycleKit.framework/Modules/CycleKit.swiftmodule)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/CycleKit.framework/Headers)

// RUN: split-file %s %t

// RUN: cp %t/inputs/A.swiftinterface %t/mock.sdk/System/Library/Frameworks/A.framework/Modules/A.swiftmodule/%target-swiftinterface-name
// RUN: cp %t/inputs/framework_A.modulemap %t/mock.sdk/System/Library/Frameworks/A.framework/Modules/module.modulemap
// RUN: cp %t/inputs/A-framework.h %t/mock.sdk/System/Library/Frameworks/A.framework/Headers/A.h

// RUN: cp %t/inputs/CycleKit.swiftinterface %t/mock.sdk/System/Library/Frameworks/CycleKit.framework/Modules/CycleKit.swiftmodule/%target-swiftinterface-name
// RUN: cp %t/inputs/framework_CycleKit.modulemap %t/mock.sdk/System/Library/Frameworks/CycleKit.framework/Modules/module.modulemap
// RUN: cp %t/inputs/CycleKit.h %t/mock.sdk/System/Library/Frameworks/CycleKit.framework/Headers/CycleKit.h

// Non-SDK dependency shadowing
// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %t/inputs -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-name CycleKit  &> %t/out.txt
// RUN: %FileCheck --check-prefix=CHECK-NONSDK %s < %t/out.txt

// CHECK-NONSDK: note: source target 'CycleKit' shadowing a Swift module with the same name at: '{{.*}}{{/|\\}}diagnose_dependency_cycle_shadow.swift.tmp{{/|\\}}inputs'

// SDK dependency shadowing
// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-name CycleKit -sdk %t/mock.sdk &> %t/out-sdk.txt
// RUN: %FileCheck --check-prefix=CHECK-SDK %s < %t/out-sdk.txt

// CHECK-SDK: note: source target 'CycleKit' shadowing an SDK Swift module with the same name at: '{{.*}}{{/|\\}}mock.sdk{{/|\\}}System{{/|\\}}Library{{/|\\}}Frameworks{{/|\\}}CycleKit.framework{{/|\\}}Modules{{/|\\}}CycleKit.swiftmodule'

//--- test.swift
import A

//--- inputs/CycleKit.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name CycleKit -enable-library-evolution

public func CycleKitFunc() {}

//--- inputs/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -enable-library-evolution
@_exported import A
public func AFunc() {}

//--- inputs/A.h
#import <CycleKit.h>
void funcA(void);

//--- inputs/A-framework.h
#import <CycleKit/CycleKit.h>
void funcA(void);

//--- inputs/CycleKit.h
void funcCycleKit(void);

//--- inputs/module.modulemap
module A {
    header "A.h"
    export *
}

module CycleKit {
    header "CycleKit.h"
    export *
}

//--- inputs/framework_A.modulemap
framework module A {
    header "A.h"
    export *
}

//--- inputs/framework_CycleKit.modulemap
framework module CycleKit {
    header "CycleKit.h"
    export *
}
