// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/out.h
// RUN: %FileCheck %s < %t/out.h

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: @interface PublicClass : NSObject{{$}}
public class PublicClass: NSObject {
  // CHECK-NEXT: - (void)spiMember;
  @_spi(A) @objc public func spiMember() {}
  // CHECK-NEXT: init
} // CHECK-NEXT: @end

// CHECK-LABEL: @interface SPIClass : NSObject{{$}}
@_spi(B)
public class SPIClass: NSObject {
  // CHECK-NEXT: - (void)publicMember;
  @objc public func publicMember() {}
  // CHECK-NEXT: init
} // CHECK-NEXT: @end
