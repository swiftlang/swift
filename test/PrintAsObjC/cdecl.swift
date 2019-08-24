// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-doc -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/cdecl.swiftmodule -typecheck -emit-objc-header-path %t/cdecl.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/cdecl.h
// RUN: %check-in-clang %t/cdecl.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/cdecl.h -include ctypes.h -include CoreFoundation.h

// REQUIRES: objc_interop

// CHECK: /// What a nightmare!
// CHECK-LABEL: double (^ _Nonnull block_nightmare(SWIFT_NOESCAPE float (^ _Nonnull x)(NSInteger)))(char) SWIFT_WARN_UNUSED_RESULT;

/// What a nightmare!
@_cdecl("block_nightmare")
public func block_nightmare(x: @convention(block) (Int) -> Float)
  -> @convention(block) (CChar) -> Double { return { _ in 0 } }

// CHECK-LABEL: double (^ _Nonnull block_recurring_nightmare(float (^ _Nonnull x)(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(double))))(SWIFT_NOESCAPE char (^ _Nonnull)(unsigned char)) SWIFT_WARN_UNUSED_RESULT;
@_cdecl("block_recurring_nightmare")
public func block_recurring_nightmare(x: @escaping @convention(block) (@convention(block) (Double) -> Int) -> Float)
  -> @convention(block) (_ asdfasdf: @convention(block) (CUnsignedChar) -> CChar) -> Double {
  fatalError()
}

// CHECK-LABEL: void foo_bar(NSInteger x, NSInteger y);
@_cdecl("foo_bar")
func foo(x: Int, bar y: Int) {}

// CHECK-LABEL: double (* _Nonnull function_pointer_nightmare(SWIFT_NOESCAPE float (* _Nonnull x)(NSInteger)))(char) SWIFT_WARN_UNUSED_RESULT;
@_cdecl("function_pointer_nightmare")
func function_pointer_nightmare(x: @convention(c) (Int) -> Float)
  -> @convention(c) (CChar) -> Double { return { _ in 0 } }

// CHECK-LABEL: double (* _Nonnull function_pointer_recurring_nightmare(float (* _Nonnull x)(SWIFT_NOESCAPE NSInteger (* _Nonnull)(double))))(SWIFT_NOESCAPE char (* _Nonnull)(unsigned char)) SWIFT_WARN_UNUSED_RESULT;
@_cdecl("function_pointer_recurring_nightmare")
public func function_pointer_recurring_nightmare(x: @escaping @convention(c) (@convention(c) (Double) -> Int) -> Float)
  -> @convention(c) (@convention(c) (CUnsignedChar) -> CChar) -> Double {
  fatalError()
}
  
// CHECK-LABEL: void has_keyword_arg_names(NSInteger auto_, NSInteger union_);
@_cdecl("has_keyword_arg_names")
func keywordArgNames(auto: Int, union: Int) {}

@objc
class C {}

// CHECK-LABEL: C * _Null_unspecified return_iuo(void) SWIFT_WARN_UNUSED_RESULT;
@_cdecl("return_iuo")
func returnIUO() -> C! { return C() }

// CHECK-LABEL: void return_never(void) SWIFT_NORETURN;
@_cdecl("return_never")
func returnNever() -> Never { fatalError() }

// CHECK-LABEL: void takes_iuo(C * _Null_unspecified c);
@_cdecl("takes_iuo")
func takesIUO(c: C!) {}
