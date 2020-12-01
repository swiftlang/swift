/// Test the warning on an SPI import of the public interface of a module.

// RUN: %empty-directory(%t)

/// Compile the SPI lib.
// RUN: %target-swift-frontend -enable-experimental-prespecialization -emit-module %S/Inputs/spi_helper.swift -module-name SPIHelper -emit-module-path %t/SPIHelper.swiftmodule -emit-module-interface-path %t/SPIHelper.swiftinterface -emit-private-module-interface-path %t/SPIHelper.private.swiftinterface -enable-library-evolution -swift-version 5 -parse-as-library

/// Reading from swiftmodule, no warning.
// RUN: %target-swift-frontend -typecheck %s -I %t

/// Reading from .private.swiftinterface, no warning.
// RUN: rm %t/SPIHelper.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -I %t

/// Reading from the public .swiftinterface should produce the warning.
// RUN: rm %t/SPIHelper.private.swiftinterface
// RUN: %target-typecheck-verify-swift -I %t

@_spi(SPIHelper) import SPIHelper // expected-warning {{'@_spi' import of 'SPIHelper' will not include any SPI symbols; 'SPIHelper' was built from the public interface at}}
