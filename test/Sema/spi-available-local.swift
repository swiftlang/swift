// REQUIRES: OS=ios
// REQUIRES: rdar91325474
// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -target x86_64-apple-macosx11.9 -parse-as-library %s -typecheck -library-level=api >& %t/macos.txt
// RUN: %FileCheck %s < %t/macos.txt

// RUN: %target-swift-frontend -target arm64-apple-ios13.0  -parse-as-library %s -typecheck -library-level=api

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public class SPIClass {}

public func foo(_ c: SPIClass) {}

// CHECK: cannot use class 'SPIClass' here; it is SPI
