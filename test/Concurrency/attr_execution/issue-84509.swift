// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
// RUN:   -enable-upcoming-feature NonisolatedNonsendingByDefault \
// RUN:   -module-name main -I %t -verify -verify-ignore-unrelated \
// RUN:   -parse-as-library -o %t/main.o

// REQUIRES: objc_interop
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

//--- Test.h
@import Foundation;

@protocol InterfaceT
@end

@interface Base : NSObject
- (void)prepareWithP:(nullable id<InterfaceT>)p completionHandler:(void (^ _Nonnull)(NSError * _Nullable))completionHandler;
@end

//--- main.swift
import Foundation

class Derived : Base {
  nonisolated(nonsending) override public func prepare(withP: (any InterfaceT)?) async throws {}
}

