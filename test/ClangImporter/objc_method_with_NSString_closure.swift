// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not --crash %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-silgen %t/Theme.swift -import-objc-header %t/Theme.h

//--- Theme.h
#import <Foundation.h>

typedef struct ThemeFuncTable {
  NSString *_Nonnull (*_Nonnull keyGetter)();
} ThemeFuncTable;

//--- Theme.swift
import Theme

let _ = ThemeFuncTable(
    keyGetter: { "SomeTheme" }
)
