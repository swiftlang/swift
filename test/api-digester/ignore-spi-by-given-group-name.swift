// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-before.json %s -enable-library-evolution -DBASELINE -emit-tbd-path %t/abi-before.tbd
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-after.json %s -enable-library-evolution -emit-tbd-path %t/abi-after.tbd
// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/result.txt
// RUN: %FileCheck %s -check-prefix CHECK_INCLUDE_SPI < %t/result.txt

// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/result.txt -ignore-spi-group secret
// RUN: %FileCheck -check-prefix CHECK_EXCLUDE_SPI %s < %t/result.txt

#if BASELINE

@_spi(secret)
public func foo() {}

#else


#endif

// CHECK_INCLUDE_SPI: Func foo() has been removed
// CHECK_EXCLUDE_SPI-NOT: foo()
