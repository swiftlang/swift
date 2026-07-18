// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan
 
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -target %target-swift-6.2-abi-triple \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

#define SWIFT_REFERENCE \
    __attribute__((swift_attr("import_reference"))) \
    __attribute__((swift_attr("retain:immortal")))  \
    __attribute__((swift_attr("release:immortal")))

struct ValueType {
  // expected-expansion@+8:56{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valBasic(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe valBasic(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int valBasic(const int * __counted_by(len) p, int len) const;

  // expected-expansion@+14:80{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valNoescape(_ p: Span<CInt>) {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    return unsafe valNoescape(_pPtr.baseAddress, len)|}}
  //   expected-remark@12{{macro content: |}|}}
  // }}
  void valNoescape(const int * __counted_by(len) p [[clang::noescape]], int len) const;

  // expected-expansion@+8:64{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public mutating func valNonconstSelf(_ p: UnsafeBufferPointer<CInt>) {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe valNonconstSelf(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  void valNonconstSelf(const int * __counted_by(len) p, int len);

  // expected-expansion@+8:68{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valBasicVirt(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe valBasicVirt(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  virtual int valBasicVirt(const int * __counted_by(len) p, int len) const;

  // expected-expansion@+19:103{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valLifetimebound(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe valLifetimebound(_pPtr.baseAddress, len)|}}
  //   expected-remark@12{{macro content: |    if unsafe _resultValue == nil {|}}
  //   expected-remark@13{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
  //   expected-remark@14{{macro content: |      return MutableSpan<CInt>()|}}
  //   expected-remark@15{{macro content: |    }|}}
  //   expected-remark@16{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
  //   expected-remark@17{{macro content: |}|}}
  // }}
  int * __counted_by(len) valLifetimebound(int * __counted_by(len) p [[clang::lifetimebound]], int len) const;

  // expected-expansion@+19:130{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valLifetimeboundVirtual(_ p: Span<CInt>) -> Span<CInt> {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    let _resultValue: UnsafePointer<CInt>? = unsafe valLifetimeboundVirtual(_pPtr.baseAddress, len)|}}
  //   expected-remark@12{{macro content: |    if unsafe _resultValue == nil {|}}
  //   expected-remark@13{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
  //   expected-remark@14{{macro content: |      return Span<CInt>()|}}
  //   expected-remark@15{{macro content: |    }|}}
  //   expected-remark@16{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
  //   expected-remark@17{{macro content: |}|}}
  // }}
  virtual const int * __counted_by(len) valLifetimeboundVirtual(const int * __counted_by(len) p [[clang::lifetimebound]], int len) const;

  // expected-expansion@+12:55{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public borrowing func valLifetimeboundSelf(_ len: CInt) -> MutableSpan<CInt> {|}}
  //   expected-remark@4{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe valLifetimeboundSelf(len)|}}
  //   expected-remark@5{{macro content: |    if unsafe _resultValue == nil {|}}
  //   expected-remark@6{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
  //   expected-remark@7{{macro content: |      return MutableSpan<CInt>()|}}
  //   expected-remark@8{{macro content: |    }|}}
  //   expected-remark@9{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
  //   expected-remark@10{{macro content: |}|}}
  // }}
  int * __counted_by(len) valLifetimeboundSelf(int len) const [[clang::lifetimebound]];
};

// FIXME: inheriting from type with safe wrapper crashes compiler
// struct InheritValue : public ValueType {
//   int valSubBasic(const int * __counted_by(len) p, int len) const;
  
//   // no lifetimebound annotation on override
//   const int * __counted_by(len) valLifetimeboundVirtual(const int * __counted_by(len) p, int len) const override;
// };

struct SWIFT_REFERENCE RefType {
  // expected-expansion@+8:56{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refBasic(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe refBasic(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int refBasic(const int * __counted_by(len) p, int len) const;

  // expected-expansion@+16:80{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   FIXME: don't attach availability if surrounding context has more specific availability
  //   expected-error@2{{instance method cannot be more available than enclosing scope}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refNoescape(_ p: Span<CInt>) {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    return unsafe refNoescape(_pPtr.baseAddress, len)|}}
  //   expected-remark@12{{macro content: |}|}}
  // }}
  void refNoescape(const int * __counted_by(len) p [[clang::noescape]], int len) const;

  // expected-expansion@+8:64{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refNonconstSelf(_ p: UnsafeBufferPointer<CInt>) {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe refNonconstSelf(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  void refNonconstSelf(const int * __counted_by(len) p, int len);

  // FIXME: no safe wrapper generated for virtual thunk on ref counted type
  virtual int refBasicVirt(const int * __counted_by(len) p, int len) const;

  // expected-expansion@+20:103{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   FIXME: don't attach availability if surrounding context has more specific availability
  //   expected-error@2{{instance method cannot be more available than enclosing scope}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refLifetimebound(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe refLifetimebound(_pPtr.baseAddress, len)|}}
  //   expected-remark@12{{macro content: |    if unsafe _resultValue == nil {|}}
  //   expected-remark@13{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
  //   expected-remark@14{{macro content: |      return MutableSpan<CInt>()|}}
  //   expected-remark@15{{macro content: |    }|}}
  //   expected-remark@16{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
  //   expected-remark@17{{macro content: |}|}}
  // }}
  int * __counted_by(len) refLifetimebound(int * __counted_by(len) p [[clang::lifetimebound]], int len) const;

  virtual const int * __counted_by(len) refLifetimeboundVirtual(const int * __counted_by(len) p [[clang::lifetimebound]], int len) const;

  // expected-expansion@+7:55{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public borrowing final func refLifetimeboundSelf(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {|}}
  //   expected-remark@4{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe refLifetimeboundSelf(len), count: Int(len))|}}
  //   expected-remark@5{{macro content: |}|}}
  // }}
  int * __counted_by(len) refLifetimeboundSelf(int len) const [[clang::lifetimebound]];
};

// struct InheritRef : public RefType {
//   int refSubBasic(const int * __counted_by(len) p, int len) const;
  
//   // no lifetimebound annotation on override
//   const int * __counted_by(len) refLifetimeboundVirtual(const int * __counted_by(len) p, int len) const override;
// };

// struct InheritRefPrivate : RefType {
//   int refPrivateSubBasic(const int * __counted_by(len) p, int len) const;
  
//   // with lifetimebound annotation on override
//   const int * __counted_by(len) refLifetimeboundVirtual(const int * __counted_by(len) p [[clang::lifetimebound]], int len) const override;
// };

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t -source-filename=x -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 0d713e7cffaa8d785c00f4367f99c1b8751e1bbba85aa5a91da13a33602d43d9
import Test


func call_valBasic(_ self: ValueType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.valBasic(p, len)
}

func call_valNoescape(_ self: ValueType, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.valNoescape(p, len)
}
func call_valNonconstSelf(_ self: inout ValueType, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.valNonconstSelf(p, len)
}

func call_valBasicVirt(_ self: ValueType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.valBasicVirt(p, len)
}

func call_valLifetimebound(_ self: ValueType, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe self.valLifetimebound(p, len)
}

func call_valLifetimeboundVirtual(_ self: ValueType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> UnsafePointer<CInt>! {
  return unsafe self.valLifetimeboundVirtual(p, len)
}
func call_valLifetimeboundSelf(_ self: ValueType, _ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe self.valLifetimeboundSelf(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_valNonconstSelf(_ self: inout ValueType, _ p: UnsafeBufferPointer<CInt>) {
  return unsafe self.valNonconstSelf(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_valBasic(_ self: ValueType, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.valBasic(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_valBasicVirt(_ self: ValueType, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.valBasicVirt(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_valNoescape(_ self: ValueType, _ p: Span<CInt>) {
  return self.valNoescape(p)
}

func call_refBasicVirt(_ self: RefType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasicVirt(p, len)
}

func call_refLifetimeboundVirtual(_ self: RefType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> UnsafePointer<CInt>! {
  return unsafe self.refLifetimeboundVirtual(p, len)
}

func call_refBasic(_ self: RefType, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasic(p, len)
}

func call_refNoescape(_ self: RefType, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNoescape(p, len)
}

func call_refNonconstSelf(_ self: RefType, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNonconstSelf(p, len)
}

func call_refLifetimebound(_ self: RefType, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe self.refLifetimebound(p, len)
}
func call_refLifetimeboundSelf(_ self: RefType, _ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe self.refLifetimeboundSelf(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refBasic(_ self: RefType, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.refBasic(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refNonconstSelf(_ self: RefType, _ p: UnsafeBufferPointer<CInt>) {
  return unsafe self.refNonconstSelf(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_refNoescape(_ self: RefType, _ p: Span<CInt>) {
  return self.refNoescape(p)
}
