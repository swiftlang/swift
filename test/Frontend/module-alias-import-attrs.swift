/// Test various import attributes with module aliasing.
///
/// Module 'Lib' imports module 'XLogging' via module aliasing and with various import attributes.
/// Module 'User' imports 'Lib'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Below, compile module Lib that imports XLogging with various import attributes via
/// -module-alias XLogging=AppleLogging

/// Test @_spi: Should fail
// RUN: not %target-swift-frontend -module-name Lib1 %t/FileLib1.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib1.swiftmodule 2> %t/result-Lib1.output
// RUN: %FileCheck %s -input-file %t/result-Lib1.output -check-prefix CHECK-SPI-ERROR
// CHECK-SPI-ERROR: error: 'spiFunc' is inaccessible due to '@_spi' protection level

/// Test @_spi: Should pass by adding @_spi
// RUN: %target-swift-frontend -module-name Lib2 %t/FileLib2.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib2.swiftmodule
// RUN: test -f %t/Lib2.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/Lib2.swiftmodule > %t/Lib2.dump.txt
// RUN: %FileCheck -check-prefix=CHECK-REAL-NAME %s < %t/Lib2.dump.txt
// CHECK-REAL-NAME-NOT: XLogging
// CHECK-REAL-NAME: AppleLogging

/// Test @_implementationOnly: Should fail
// RUN: not %target-swift-frontend -module-name Lib3 %t/FileLib3.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib3.swiftmodule 2> %t/result-Lib3.output
// RUN: %FileCheck %s -input-file %t/result-Lib3.output -check-prefix CHECK-IMPL-ERROR
// CHECK-IMPL-ERROR: error: cannot use module 'XLogging' here; 'XLogging' has been imported as implementation-only


/// Test @_private: Should pass

/// Create AppleLoggingEnablePrivate.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLoggingEnablePrivate -module-alias XLogging=AppleLoggingEnablePrivate %S/Inputs/module-alias-src.swift -emit-module -emit-module-path %t/AppleLoggingEnablePrivate.swiftmodule -enable-private-imports -enable-implicit-dynamic
// RUN: test -f %t/AppleLoggingEnablePrivate.swiftmodule

// RUN: %target-swift-frontend -module-name Lib4 %t/FileLib4.swift -module-alias XLogging=AppleLoggingEnablePrivate -I %t -emit-module -emit-module-path %t/Lib4.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/Lib4.swiftmodule > %t/Lib4.dump.txt
// RUN: %FileCheck -check-prefix=CHECK-REAL-NAME4 %s < %t/Lib4.dump.txt
// CHECK-REAL-NAME4-NOT: XLogging
// CHECK-REAL-NAME4: AppleLoggingEnablePrivate

/// Test @testable: Should pass

/// Create AppleLoggingEnableTesting.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLoggingEnableTesting -module-alias XLogging=AppleLoggingEnableTesting %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLoggingEnableTesting.swiftmodule -enable-testing
// RUN: test -f %t/AppleLoggingEnableTesting.swiftmodule

// RUN: %target-swift-frontend -module-name Lib5 %t/FileLib5.swift -module-alias XLogging=AppleLoggingEnableTesting -I %t -emit-module -emit-module-path %t/Lib5.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/Lib5.swiftmodule > %t/Lib5.dump.txt
// RUN: %FileCheck -check-prefix=CHECK-REAL-NAME5 %s < %t/Lib5.dump.txt
// CHECK-REAL-NAME5-NOT: XLogging
// CHECK-REAL-NAME5: AppleLoggingEnableTesting

/// Test import struct: Should pass with correct module name reference
// RUN: %target-swift-frontend -module-name Lib6 %t/FileLib6.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib6.swiftmodule -c -o %t/Lib6.o
// RUN: llvm-nm --defined-only %t/Lib6.o > %t/Lib6.dump.txt
// RUN: %FileCheck -check-prefix=CHECK-REAL-NAME6 %s < %t/Lib6.dump.txt
// CHECK-REAL-NAME6-NOT: XLogging
// CHECK-REAL-NAME6: s4Lib65start12AppleLogging6LoggerVyF

/// Test canImport
// RUN: %target-swift-frontend -module-name Lib7 %t/FileLib7.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib7.swiftmodule -Rmodule-loading 2> %t/Lib7.dump.txt

// RUN: %FileCheck -check-prefix=CHECK-LOAD %s < %t/Lib7.dump.txt
// CHECK-LOAD-NOT: XLogging.swiftmodule
// CHECK-LOAD: AppleLogging.swiftmodule

/// Test @_exported: Should pass with correct module name reference
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: %target-swift-frontend -module-name User %t/FileUser.swift -I %t -emit-module -emit-module-path %t/User.swiftmodule -c -o %t/User.o
// RUN: test -f %t/User.swiftmodule

// RUN: llvm-nm --defined-only %t/User.o > %t/User.dump.txt
// RUN: %FileCheck -check-prefix=CHECK-REAL-NAME-USER %s < %t/User.dump.txt
// CHECK-REAL-NAME-USER-NOT: XLogging
// CHECK-REAL-NAME-USER: s4User04MainA0V3use12AppleLogging6LoggerVyF

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
  @_spi(Usable) public func spiFunc() {}
  public func regularFunc() {}
  func internalFunc() {}
  private func privateFunc() {}
}
struct InternalLogger {
  init() {}
}
private struct PrivateLogger {
  init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib1.swift
import XLogging

public func start() {
  let it = Logger()
  it.regularFunc()
  it.spiFunc() // should fail
}

// BEGIN FileLib2.swift
@_spi(Usable) import XLogging

public func start() {
  let it = Logger()
  it.regularFunc()
  it.spiFunc() // pass
}

// BEGIN FileLib3.swift
@_implementationOnly import XLogging

public func start() -> XLogging.Logger? { // should fail
  return Logger()
}

// BEGIN FileLib4.swift
@_private(sourceFile: "module-alias-src.swift")
import XLogging

public func start() {
  let x = InternalLogger() // should pass
  let y = PrivateLogger() // should pass
  Logger().internalFunc() // should pass
  Logger().privateFunc() // should pass
  print(x, y)
}

// BEGIN FileLib5.swift
@testable import XLogging

public func start() {
  let x = InternalLogger() // should pass
  print(x)
}

// BEGIN FileLib6.swift
import struct XLogging.Logger // should map to AppleLogging.Logger

public func start() -> XLogging.Logger {
  let x = Logger()
  x.regularFunc()
  return x
}

// BEGIN FileLib7.swift
#if canImport(XLogging)
import XLogging // should map to AppleLogging.Logger
#endif

#if canImport(XLogging)
public func start() -> XLogging.Logger {
  return Logger()
}
#endif
public func end() {}

// BEGIN FileLib.swift
@_exported import XLogging

public func start() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileUser.swift
import Lib

public struct MainUser {
  public func use() -> Logger {
    return Logger() // should map to AppleLogging.Logger
  }
}

