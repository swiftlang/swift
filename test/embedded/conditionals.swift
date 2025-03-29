// RUN: %target-swift-emit-ir %s -parse-stdlib | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded | %FileCheck %s --check-prefix EMBEDDED

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

#if $Embedded
public func embedded() { }
#else
public func regular() { }
#endif

// CHECK:    define {{.*}}void @"$s12conditionals7regularyyF"()
// EMBEDDED: define {{.*}}void @"$e12conditionals8embeddedyyF"()
