// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -emit-module -module-name Module0 -DSPI_DECLARING_MODULE -o %t/Module0.swiftmodule
// RUN: %target-build-swift %s -module-name Module1 -DSPI_IMPORTING_MODULE -emit-loaded-module-trace -o %t/spi_import -I %t
// RUN: %FileCheck %s < %t/Module1.trace.json

#if SPI_DECLARING_MODULE

@_spi(ForModule1)
public func f() {}

#endif

#if SPI_IMPORTING_MODULE

@_spi(ForModule1)
import Module0

#endif

// CHECK: "name":"Module0","path":"{{[^"]*}}","isImportedDirectly":true
