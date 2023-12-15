// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// A package swiftinterface should be generated only if -package-name is passed.
// RUN: %target-build-swift -emit-module %t/Bar.swift -I %t \
// RUN:   -module-name Bar -package-name foopkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Bar.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Bar.package.swiftinterface

// RUN: %FileCheck %s < %t/Bar.package.swiftinterface
// CHECK: -package-name foopkg
// CHECK: public struct PubStruct
// CHECK: package struct PkgStruct

/// If -package-name is not passed, a package interface should not be generated even if 
/// `-emit-package-module-interface-path` is passed.
// RUN: %target-build-swift -emit-module %t/Baz.swift -I %t \
// RUN:   -module-name Baz \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Baz.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Baz.package.swiftinterface

// RUN: ls %t | not grep "Baz.package.swiftinterface"

//--- Bar.swift
public struct PubStruct {

}

package struct PkgStruct {

}

//--- Baz.swift
public struct PubStruct {

}
