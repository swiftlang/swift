// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: objc_interop

// Build two Clang modules containing equivalent definitions of the same
// Objective-C interface and protocol.
// RUN: %target-swift-emit-pcm -module-name A -o %t/A.pcm \
// RUN:   %t/module.modulemap
// RUN: %target-swift-emit-pcm -module-name B -o %t/B.pcm \
// RUN:   %t/module.modulemap

// Build each Swift module with only its corresponding Clang module loaded, so
// their serialized cross-references name different owning modules.
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name SwiftA -emit-module-path %t/SwiftA.swiftmodule \
// RUN:   %t/SwiftA.swift -I %t -module-cache-path %t/cache-swift-a \
// RUN:   -Xcc -fmodule-file=A=%t/A.pcm
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name SwiftB -emit-module-path %t/SwiftB.swiftmodule \
// RUN:   %t/SwiftB.swift -I %t -module-cache-path %t/cache-swift-b \
// RUN:   -Xcc -fmodule-file=B=%t/B.pcm

// Only one declaration can remain the primary definition. References through
// both modules must resolve through the merged Objective-C redeclaration
// chains.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -module-cache-path %t/cache-client \
// RUN:   -Xcc -fmodule-file=A=%t/A.pcm \
// RUN:   -Xcc -fmodule-file=B=%t/B.pcm

//--- module.modulemap
module A {
  header "A.h"
}
module B {
  header "B.h"
}

//--- A.h
@interface SharedClass
@end

@protocol SharedProtocol
@end

//--- B.h
@interface SharedClass
@end

@protocol SharedProtocol
@end

//--- SwiftA.swift
import A

public typealias ClassFromA = A.SharedClass
public typealias ProtocolFromA = A.SharedProtocol

//--- SwiftB.swift
import B

public typealias ClassFromB = B.SharedClass
public typealias ProtocolFromB = B.SharedProtocol

//--- Client.swift
import A
import B
import SwiftA
import SwiftB

func useA(_: A.SharedClass) {}
func useB(_: B.SharedClass) {}
func useSwiftA(_: SwiftA.ClassFromA) {}
func useSwiftB(_: SwiftB.ClassFromB) {}
func useAProtocol(_: A.SharedProtocol) {}
func useBProtocol(_: B.SharedProtocol) {}
func useSwiftAProtocol(_: SwiftA.ProtocolFromA) {}
func useSwiftBProtocol(_: SwiftB.ProtocolFromB) {}
