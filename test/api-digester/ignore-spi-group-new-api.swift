// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-before.json %s -enable-library-evolution -DBASELINE -emit-tbd-path %t/abi-before.tbd -tbd-install_name Foo
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-after.json %s -enable-library-evolution -emit-tbd-path %t/abi-after.tbd -tbd-install_name Foo
// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/include-result.txt
// RUN: %FileCheck %s -check-prefix CHECK_INCLUDE_SPI < %t/include-result.txt

// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/exclude-all-result.txt -ignore-spi-group secret
// RUN: %FileCheck -check-prefix CHECK_EXCLUDE_ALL_SPI %s < %t/exclude-all-result.txt

// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/exclude-new-api-result.txt -ignore-spi-group-new-api secret
// RUN: %FileCheck -check-prefix CHECK_EXCLUDE_NEW_API %s < %t/exclude-new-api-result.txt

#if BASELINE

@_spi(secret)
public func foo() {}

#else

public func bar() {}

@_spi(secret)
public func baz() {}

#endif

// CHECK_INCLUDE_SPI: Func foo() has been removed
// CHECK_EXCLUDE_ALL_SPI-NOT: foo()
// CHECK_EXCLUDE_NEW_API: Func foo() has been removed

// CHECK_INCLUDE_SPI: Func bar() is a new API without '@available'
// CHECK_EXCLUDE_ALL_SPI: Func bar() is a new API without '@available'
// CHECK_EXCLUDE_NEW_API: Func bar() is a new API without '@available'

// CHECK_INCLUDE_SPI: Func baz() is a new API without '@available'
// CHECK_EXCLUDE_ALL_SPI-NOT: baz()
// CHECK_EXCLUDE_NEW_API-NOT: baz()

