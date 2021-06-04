// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SPI -I %t -pretty-print -output-dir %t -include-spi-symbols
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPI

// RUN: %target-swift-symbolgraph-extract -module-name SPI -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix NOSPI

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -include-spi-symbols -v
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPI-COMPILE

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix NOSPI-COMPILE

@_spi(SPI) public struct SomeStruct {}

// SPI: "precise": "s:3SPI10SomeStructV"
// SPI: "spi": true

// NOSPI-NOT: "precise": "s:3SPI10SomeStructV"

// SPI-COMPILE: s:3SPI10SomeStructV
// NOSPI-COMPILE-NOT: s:3SPI10SomeStructV
