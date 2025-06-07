// RUN: %empty-directory(%t)

// RUN: %target-swift-typecheck-module-from-interface(%S/Inputs/opaque-type-abi-name/Bottom.swiftinterface) -module-name Bottom -o %t/Bottom.swiftmodule -verify
// RUN: %target-swift-typecheck-module-from-interface(%S/Inputs/opaque-type-abi-name/Top.swiftinterface) -module-name Top -o %t/Top.swiftmodule -verify -I %S/Inputs/opaque-type-abi-name
