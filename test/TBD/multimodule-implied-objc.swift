// REQUIRES: VENDOR=apple
// XFAIL: OS=xros
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: split-file %s %t

/// Create Swift modules to import.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -module-name IndirectMixedDependency -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -emit-module %t/IndirectMixedDependency.swift \
// RUN:   -emit-module-path %t/IndirectMixedDependency.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -enable-library-evolution \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -emit-module %t/SwiftDependency.swift \
// RUN:   -module-name SwiftDependency -I %t\
// RUN:   -emit-module-path %t/SwiftDependency.swiftmodule

// Generate TBD file.
// RUN: %target-swift-frontend -I %t -module-cache-path %t/cache \
// RUN:   %t/Client.swift -emit-ir -o/dev/null -parse-as-library \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -module-name client -validate-tbd-against-ir=missing \
// RUN:   -tbd-install_name client -emit-tbd -emit-tbd-path %t/client.tbd 

// RUN: %validate-json %t/client.tbd | %FileCheck %s

// CHECK-NOT: "CLASS_$__TtCO6client11extendedAPI6Square"
// CHECK: "objc_class"
// CHECK: "_TtCO6client11extendedAPI6Square"

//--- module.modulemap
module IndirectMixedDependency {
  header "IndirectMixedDependency.h"
}

//--- IndirectMixedDependency.h
@import Foundation;
@interface Shape : NSObject
@end

//--- IndirectMixedDependency.swift
@_exported import IndirectMixedDependency

//--- SwiftDependency.swift
import IndirectMixedDependency 
open class Rectangle : IndirectMixedDependency.Shape {}


//--- Client.swift
import SwiftDependency

public enum extendedAPI {}
extension extendedAPI {
    public class Square : SwiftDependency.Rectangle {}
}
