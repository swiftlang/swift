// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/Library.swift -sil-verify-all -emit-sil -import-objc-header %t/Header.h > /dev/null

// REQUIRES: objc_interop

//--- Header.h

@import Foundation;

@interface Foo : NSObject
@property (nonatomic, readwrite) NSUInteger length;
@end

//--- Library.swift

func foo(_ x: borrowing Foo) -> UInt {
    let y = x.length
    return y
}

