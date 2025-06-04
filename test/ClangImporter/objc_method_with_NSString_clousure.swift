// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-silgen %t/Theme.swift -import-objc-header %t/Theme.h | %FileCheck %t/Theme.swift

// REQUIRES: objc_interop

//--- Theme.h
#import <Foundation/Foundation.h>

typedef struct ThemeFuncTable {
  NSString *_Nonnull (*_Nonnull keyGetter)();
} ThemeFuncTable;

//--- Theme.swift
import Theme

let _ = ThemeFuncTable(
    keyGetter: { "SomeTheme" }
)
