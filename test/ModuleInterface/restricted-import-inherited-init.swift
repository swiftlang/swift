// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Inputs/implementation-only-init
// RUN: split-file %s %t

// Test with 'internal import' syntax
// RUN: %target-swift-frontend -emit-module %t/test.swift -module-name Client -enable-library-evolution -swift-version 6 -I %t/Inputs/implementation-only-init -emit-module-interface-path %t/Client.swiftinterface -emit-private-module-interface-path %t/Client.private.swiftinterface

// RUN: %FileCheck %s -check-prefix=CHECK < %t/Client.private.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-PUBLIC < %t/Client.swiftinterface

// Test with '@_implementationOnly import' syntax
// RUN: %target-swift-frontend -emit-module %t/test2.swift -module-name Client2 -enable-library-evolution -swift-version 6 -I %t/Inputs/implementation-only-init -emit-module-interface-path %t/Client2.swiftinterface -emit-private-module-interface-path %t/Client2.private.swiftinterface

// RUN: %FileCheck %s -check-prefix=CHECK < %t/Client2.private.swiftinterface

// Test with '@_spiOnly import' syntax
// RUN: %target-swift-frontend -emit-module %t/test3.swift -module-name Client3 -enable-library-evolution -swift-version 6 -I %t/Inputs/implementation-only-init -emit-module-interface-path %t/Client3.swiftinterface -emit-private-module-interface-path %t/Client3.private.swiftinterface -experimental-spi-only-imports

// RUN: %FileCheck %s -check-prefix=CHECK-SPI-PRIVATE < %t/Client3.private.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-SPI-PUBLIC < %t/Client3.swiftinterface

// Test with 'package import' syntax
// RUN: %target-swift-frontend -emit-module %t/test4.swift -module-name Client4 -enable-library-evolution -swift-version 6 -I %t/Inputs/implementation-only-init -emit-module-interface-path %t/Client4.swiftinterface -emit-private-module-interface-path %t/Client4.private.swiftinterface -emit-package-module-interface-path %t/Client4.package.swiftinterface -package-name MyPackage

// RUN: %FileCheck %s -check-prefix=CHECK-PRIVATE < %t/Client4.private.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-PACKAGE < %t/Client4.package.swiftinterface

// Test with 'private import' syntax
// RUN: %target-swift-frontend -emit-module %t/test5.swift -module-name Client5 -enable-library-evolution -swift-version 6 -I %t/Inputs/implementation-only-init -emit-module-interface-path %t/Client5.swiftinterface -emit-private-module-interface-path %t/Client5.private.swiftinterface

// RUN: %FileCheck %s -check-prefix=CHECK-PRIVATE < %t/Client5.private.swiftinterface

// CHECK: import BaseModule
// CHECK-NOT: import ExtensionModule
// CHECK: public class DerivedClass
// CHECK-NOT: init(handler:
// CHECK-NOT: init(nonnullHandler:
// CHECK-NOT: ExtensionType

// CHECK-PUBLIC: import BaseModule
// CHECK-PUBLIC-NOT: import ExtensionModule
// CHECK-PUBLIC: public class DerivedClass
// CHECK-PUBLIC-NOT: init(handler:
// CHECK-PUBLIC-NOT: init(nonnullHandler:
// CHECK-PUBLIC-NOT: ExtensionType

// CHECK-SPI-PRIVATE: import BaseModule
// CHECK-SPI-PRIVATE: @_spiOnly import ExtensionModule
// CHECK-SPI-PRIVATE: public class DerivedClass
// CHECK-SPI-PRIVATE: init!(handler:
// CHECK-SPI-PRIVATE: init!(nonnullHandler
// CHECK-SPI-PRIVATE: ExtensionType

// CHECK-SPI-PUBLIC: import BaseModule
// CHECK-SPI-PUBLIC-NOT: @_spiOnly import ExtensionModule
// CHECK-SPI-PUBLIC-NOT: import ExtensionModule
// CHECK-SPI-PUBLIC: public class DerivedClass
// CHECK-SPI-PUBLIC-NOT: init(handler:
// CHECK-SPI-PUBLIC-NOT: init(nonnullHandler:
// CHECK-SPI-PUBLIC-NOT: ExtensionType

// CHECK-PACKAGE-PRIVATE: import BaseModule
// CHECK-PACKAGE-PRIVATE-NOT: import ExtensionModule
// CHECK-PACKAGE-PRIVATE: public class DerivedClass
// CHECK-PACKAGE-PRIVATE-NOT: init(handler:
// CHECK-PACKAGE-PRIVATE-NOT: init(nonnullHandler:
// CHECK-PACKAGE-PRIVATE-NOT: ExtensionType

// CHECK-PACKAGE: import BaseModule
// CHECK-PACKAGE: package import ExtensionModule
// CHECK-PACKAGE: public class DerivedClass
// CHECK-PACKAGE: init!(handler:
// CHECK-PACKAGE: init!(nonnullHandler
// CHECK-PACKAGE: ExtensionType

// CHECK-PRIVATE: import BaseModule
// CHECK-PRIVATE-NOT: import ExtensionModule
// CHECK-PRIVATE: public class DerivedClass
// CHECK-PRIVATE-NOT: init(handler:
// CHECK-PRIVATE-NOT: init(nonnullHandler:
// CHECK-PRIVATE-NOT: ExtensionType

//--- Inputs/implementation-only-init/Base.h
@import Foundation;
@interface BaseClass : NSObject
- (instancetype)initWithInfo:(id)info responder:(id)responder;
@end

//--- Inputs/implementation-only-init/module.modulemap
module BaseModule {
  header "Base.h"
  export *
}

module ExtensionModule {
  header "Extension.h"
  export *
}

//--- Inputs/implementation-only-init/Extension.h
@import Foundation;
#import "Base.h"
@interface ExtensionType : NSObject
@end
@interface BaseClass (Extension)
- (instancetype)initWithHandler:(ExtensionType *)handler responder:(id)responder;
- (instancetype)initWithNonnullHandler:(ExtensionType * _Nonnull)handler;
@end

//--- test.swift
import BaseModule
internal import ExtensionModule
public class DerivedClass : BaseClass {
}

//--- test2.swift
import BaseModule
@_implementationOnly import ExtensionModule
public class DerivedClass : BaseClass {
}

//--- test3.swift
import BaseModule
@_spiOnly import ExtensionModule
public class DerivedClass : BaseClass {
}

//--- test4.swift
import BaseModule
package import ExtensionModule
public class DerivedClass : BaseClass {
}

//--- test5.swift
import BaseModule
private import ExtensionModule
public class DerivedClass : BaseClass {
}
