// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend \
// RUN:     %t/Library.swift \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -sil-verify-all \
// RUN:     -verify \
// RUN:     -emit-sil \
// RUN:     -import-objc-header %t/Header.h \
// RUN: > /dev/null

// REQUIRES: objc_interop

// REQUIRES: swift_feature_Lifetimes

//--- Header.h

@import Foundation;

@interface Foo : NSObject
@property (nonatomic, readwrite) NSInteger length;
- (const unichar *)_fastCharacterContents NS_RETURNS_INNER_POINTER;
@end

//--- Library.swift

extension Foo {
    var utf16Span: Span<UInt16>? {
        @_lifetime(borrow self)
        borrowing get {
            // FIXME: This error shouldn't be here.
            guard let ptr = self._fastCharacterContents() else { // expected-error{{copy of noncopyable typed value}}
                return nil
            }
            let length = self.length
            let buffer = UnsafeBufferPointer(start: ptr, count: length)
            let span = unsafe Span<UInt16>(_unsafeElements: buffer)
            return _overrideLifetime(span, borrowing: self)
        }
    }
}

extension NSString {
    var utf16Span: Span<UInt16>? {
        @_lifetime(borrow self)
        borrowing get { // expected-error{{'self' is borrowed and cannot be consumed}}
            guard let ptr = CFStringGetCharactersPtr(self) else { // expected-note{{consumed here}}
                return nil
            }
            let length = self.length
            let buffer = UnsafeBufferPointer(start: ptr, count: length)
            let span = unsafe Span<UInt16>(_unsafeElements: buffer)
            return _overrideLifetime(span, borrowing: self)
        }
    }
}
