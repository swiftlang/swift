// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Frameworks/lib.framework/Modules/lib.swiftmodule
// RUN: mkdir -p %t/Frameworks/lib2.framework/Modules/lib2.swiftmodule
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-source-info -module-name lib2 -o %t/Frameworks/lib2.framework/Modules/lib2.swiftmodule/%module-target-triple.swiftmodule %t/lib2.swift -disable-experimental-string-processing -parse-stdlib
// RUN: %target-swift-frontend -emit-module -emit-module-source-info -module-name lib -o %t/Frameworks/lib.framework/Modules/lib.swiftmodule/%module-target-triple.swiftmodule %t/lib.swift -Fsystem %t/Frameworks -disable-experimental-string-processing -parse-stdlib

// RUN: %target-swift-frontend -typecheck -index-system-modules -index-ignore-stdlib -index-store-path %t/idx -Fsystem %t/Frameworks %t/main.swift -disable-deserialization-recovery -disable-experimental-string-processing -parse-stdlib

//--- main.swift
import lib

//--- lib.swift
@_implementationOnly import lib2

struct InternalS {
  func structFunc(p: Lib2S) {}
}

extension InternalS {
  func extensionFunc(p: Lib2S) {}
}

//--- lib2.swift
public struct Lib2S {}
