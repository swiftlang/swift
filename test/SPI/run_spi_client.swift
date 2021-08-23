/// Compile an SPI lib and client

// RUN: %empty-directory(%t)

/// Compile the lib with SPI decls
// RUN: %target-build-swift-dylib(%t/%target-library-name(SPIHelper)) %S/Inputs/spi_helper.swift -emit-module -emit-module-path %t/SPIHelper.swiftmodule -module-name SPIHelper -enable-library-evolution
// RUN: %target-codesign %t/%target-library-name(SPIHelper)

/// Client with SPI access
// RUN: %target-swiftc_driver -I %t -L %t %s -o %t/spi_client -lSPIHelper %target-rpath(%t)
// RUN: %target-codesign %t/spi_client
// RUN: %target-run %t/spi_client %t/%target-library-name(SPIHelper) > %t/output

// RUN: %FileCheck %s < %t/output

// REQUIRES: executable_test

@_spi(HelperSPI) import SPIHelper

publicFunc()
// CHECK: publicFunc

spiFunc()
// CHECK: spiFunc

var c = SPIClass()
// CHECK: SPIClass.init
c.spiMethod()
// CHECK: SPIClass.spiMethod
c.spiVar = "write"
print(c.spiVar)
// CHECK: write

var s = SPIStruct()
// CHECK: SPIStruct.init
s.spiMethod()
// CHECK: SPIStruct.spiMethod
s.spiVar = "write"
print(s.spiVar)
// CHECK: write

var e = SPIEnum()
// CHECK: SPIEnum.init
e.spiMethod()
// CHECK: SPIEnum.spiMethod

var ps = PublicStruct()
ps.spiMethod()
// CHECK: PublicStruct.spiMethod
