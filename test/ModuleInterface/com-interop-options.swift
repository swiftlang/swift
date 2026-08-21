// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-stdlib -swift-version 5 \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-com-interop -com-interop-model=microsoft \
// RUN:   -disable-implicit-com-module-import -module-name MicrosoftCOMOptions \
// RUN:   -emit-module -o /dev/null \
// RUN:   -emit-module-interface-path %t/MicrosoftCOMOptions.swiftinterface %s
// RUN: %FileCheck %s --check-prefix=MICROSOFT \
// RUN:   < %t/MicrosoftCOMOptions.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:   -o %t/MicrosoftCOMOptions.swiftmodule \
// RUN:   %t/MicrosoftCOMOptions.swiftinterface

// RUN: %target-swift-frontend -parse-stdlib -swift-version 5 \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-com-interop -com-interop-model=corefoundation \
// RUN:   -disable-implicit-com-module-import \
// RUN:   -module-name CoreFoundationCOMOptions \
// RUN:   -emit-module -o /dev/null \
// RUN:   -emit-module-interface-path \
// RUN:   %t/CoreFoundationCOMOptions.swiftinterface %s
// RUN: %FileCheck %s --check-prefix=COREFOUNDATION \
// RUN:   < %t/CoreFoundationCOMOptions.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:   -o %t/CoreFoundationCOMOptions.swiftmodule \
// RUN:   %t/CoreFoundationCOMOptions.swiftinterface

// MICROSOFT: swift-module-flags:
// MICROSOFT-SAME: -enable-experimental-com-interop
// MICROSOFT-SAME: -com-interop-model=microsoft

// COREFOUNDATION: swift-module-flags:
// COREFOUNDATION-SAME: -enable-experimental-com-interop
// COREFOUNDATION-SAME: -com-interop-model=corefoundation

public func value() {}
