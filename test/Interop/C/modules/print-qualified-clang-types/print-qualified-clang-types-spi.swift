// This tests verifies that SPI visible modules are understood by the machinery
// that produces .swiftinterface files.
//
// See the documentation for the print-qualified-clang-types.swift test next to
// this one for more context.

// RUN: %empty-directory(%t)
// RUN: mkdir %t/helper_module %t/spi_main_module
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/helper_module/HelperModule.swiftmodule %S/Inputs/HelperModule.swift -I %S/Inputs
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/spi_main_module/SpiMainModule.swiftmodule -emit-module-interface-path %t/spi_main_module/SpiMainModule.swiftinterface -I %t/helper_module %S/Inputs/SpiMainModule.swift -I %S/Inputs
// RUN: %FileCheck --input-file=%t/spi_main_module/SpiMainModule.swiftinterface %s
// RUN: %target-swift-frontend -typecheck -swift-version 5 %t/spi_main_module/SpiMainModule.swiftinterface -I %t/helper_module -I %S/Inputs

// CHECK: public func funcTakingForeignStruct(_ param: ForeignB.ForeignStruct)
