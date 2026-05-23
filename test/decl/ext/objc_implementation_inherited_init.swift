// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -import-objc-header %t/Header.h -enable-experimental-feature ObjCImplementation -target %target-stable-abi-triple %t/Use.swift

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation

//--- Header.h
#import <Foundation/Foundation.h>

@interface ImplBase : NSObject
@end

@interface ImplBaseSubclass : ImplBase
@end

//--- Use.swift
import Foundation

@objc @implementation extension ImplBase {
  public override init() { super.init() }
}

_ = ImplBaseSubclass()
