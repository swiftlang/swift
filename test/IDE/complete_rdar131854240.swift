// RUN: %empty-directory(%t/completions)
// RUN: split-file %s %t

// RUN: %empty-directory(%t/Mock.platform/Developer/SDKs)
// RUN: cp -r %clang-importer-sdk-path %t/Mock.platform/Developer/SDKs/Mock.sdk

// RUN: %empty-directory(%t/Mock.platform/Developer/usr/lib/Lib.swiftmodule)
// RUN: %target-swift-frontend -emit-module %t/lib.swift -module-name Lib -swift-version 5 -emit-module-path %t/Mock.platform/Developer/usr/lib/Lib.swiftmodule/%module-target-triple.swiftmodule -enable-library-evolution -emit-module-interface-path %t/Mock.platform/Developer/usr/lib/Lib.swiftmodule/%module-target-triple.swiftinterface

// RUN: %target-swift-ide-test(mock-sdk: -sdk %t/Mock.platform/Developer/SDKs/Mock.sdk -I %t/Mock.platform/Developer/usr/lib) -batch-code-completion -source-filename %t/client.swift -filecheck %raw-FileCheck -completion-output-dir %t/completions

// REQUIRES: objc_interop

// rdar://131854240 - Make sure we don't show underscored decls in non-user
// modules.

//--- lib.swift

public struct SomeNonUnderscoredType {}
public struct _SomeUnderscoredType {}

//--- client.swift

import Lib

#^TOPLEVEL?check=TOPLEVEL;check=TOPLEVEL_NOT^#
// TOPLEVEL:         Decl[Struct]/OtherModule[Lib]/IsSystem: SomeNonUnderscoredType[#SomeNonUnderscoredType#]
// TOPLEVEL_NOT-NOT: _SomeUnderscoredType
