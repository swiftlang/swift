// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-doc -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/cdecl.swiftmodule -parse -emit-objc-header-path %t/cdecl.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/cdecl.h
// RUN: %check-in-clang %t/cdecl.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/cdecl.h -include Foundation.h -include ctypes.h -include CoreFoundation.h

// REQUIRES: objc_interop

// CHECK: /**
// CHECK-NEXT: What a nightmare!
// CHECK: */
// CHECK-LABEL: double (^ _Nonnull block_nightmare(float (^ _Nonnull x)(NSInteger)))(char);

/// What a nightmare!
@_cdecl("block_nightmare")
public func block_nightmare(x: @convention(block) (Int) -> Float)
  -> @convention(block) (CChar) -> Double { return { _ in 0 } }

// CHECK-LABEL: void foo_bar(NSInteger x, NSInteger y);
@_cdecl("foo_bar")
func foo(x: Int, bar y: Int) {}

// CHECK-LABEL: double (* _Nonnull function_pointer_nightmare(float (* _Nonnull x)(NSInteger)))(char);
@_cdecl("function_pointer_nightmare")
func function_pointer_nightmare(x: @convention(c) (Int) -> Float)
  -> @convention(c) (CChar) -> Double { return { _ in 0 } }
  
// CHECK-LABEL: void has_keyword_arg_names(NSInteger auto_, NSInteger union_);
@_cdecl("has_keyword_arg_names")
func keywordArgNames(auto: Int, union: Int) {}
