// RUN: %empty-directory(%t)

// Compilers without $CChar32IsUInt32 must see the old CChar32 definition.

// RUN: %target-swift-emit-module-interface(%t/New.swiftinterface) %s -parse-stdlib -module-name Swift
// RUN: %target-swift-typecheck-module-from-interface(%t/New.swiftinterface) -parse-stdlib -module-name Swift
// RUN: %FileCheck %s --check-prefix=NEW < %t/New.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/Old.swiftinterface) %s -parse-stdlib -module-name Swift -DOLD
// RUN: %target-swift-typecheck-module-from-interface(%t/Old.swiftinterface) -parse-stdlib -module-name Swift
// RUN: %FileCheck %s --check-prefix=OLD < %t/Old.swiftinterface

@frozen public struct UInt32 {}

public enum Unicode {}
extension Unicode {
  @frozen public struct Scalar {}
}

#if OLD
public typealias CChar32 = Unicode.Scalar
#else
public typealias CChar32 = UInt32
#endif

// Unrelated typealiases to UInt32 are printed normally.
public typealias NotCChar32 = UInt32

// NEW:      #if compiler(>=5.3) && $CChar32IsUInt32
// NEW-NEXT: public typealias CChar32 = Swift::UInt32
// NEW-NEXT: #else
// NEW-NEXT: public typealias CChar32 = Unicode.Scalar
// NEW-NEXT: #endif
// NEW-NEXT: public typealias NotCChar32 = Swift::UInt32

// OLD-NOT:  $CChar32IsUInt32
// OLD:      public typealias CChar32 = {{.*}}Unicode{{.*}}Scalar
// OLD-NEXT: public typealias NotCChar32 = Swift::UInt32
