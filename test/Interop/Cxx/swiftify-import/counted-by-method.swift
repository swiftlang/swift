// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan
 
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -target %target-swift-6.2-abi-triple \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan \
// RUN:   -verify-additional-prefix %target-vendor-

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

struct InheritValue : public ValueType {
  // expected-expansion@+8:59{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valSubBasic(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe valSubBasic(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int valSubBasic(const int * __counted_by(len) p, int len) const;
  
  // no lifetimebound annotation on override
  // expected-expansion@+8:97{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func valLifetimeboundVirtual(_ p: UnsafeBufferPointer<CInt>) -> UnsafeBufferPointer<CInt> {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe UnsafeBufferPointer<CInt>(start: unsafe valLifetimeboundVirtual(p.baseAddress, len), count: Int(len))|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  const int * __counted_by(len) valLifetimeboundVirtual(const int * __counted_by(len) p, int len) const override;
};

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

  // FIXME: merge availability with that of surrounding context
  // expected-expansion@+15:80{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-apple-error@2{{instance method cannot be more available than enclosing scope}}
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

  // FIXME: merge availability with that of surrounding context
  // expected-expansion@+20:103{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-apple-error@2{{instance method cannot be more available than enclosing scope}}
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

struct InheritRef : public RefType {
  // expected-expansion@+8:59{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refSubBasic(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe refSubBasic(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int refSubBasic(const int * __counted_by(len) p, int len) const;
  
  // no lifetimebound annotation on override
  const int * __counted_by(len) refLifetimeboundVirtual(const int * __counted_by(len) p, int len) const override;
};

struct InheritRefPrivate : RefType {
  // expected-expansion@+8:66{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func refPrivateSubBasic(_ p: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe refPrivateSubBasic(p.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int refPrivateSubBasic(const int * __counted_by(len) p, int len) const;
  
  // with lifetimebound annotation on override
  const int * __counted_by(len) refLifetimeboundVirtual(const int * __counted_by(len) p [[clang::lifetimebound]], int len) const override;
};

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
// disable auto-gen for now because the source order is not stable
// ENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t -source-filename=x -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: f8bc7adafc1f268cc0352f805f88feac123cfd1c2387c000e0a14d1a5b538e0c
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

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_valNoescape(_ self: ValueType, _ p: Span<CInt>) {
  return self.valNoescape(p)
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

func call_valBasic(_ self: InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.valBasic(p, len)
}

func call_valNoescape(_ self: InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.valNoescape(p, len)
}
func call_valNonconstSelf(_ self: inout InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.valNonconstSelf(p, len)
}

func call_valBasicVirt(_ self: InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.valBasicVirt(p, len)
}

func call_valLifetimebound(_ self: InheritValue, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.valLifetimebound(p, len)
}
func call_valLifetimeboundSelf(_ self: InheritValue, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.valLifetimeboundSelf(len)
}

func call_valSubBasic(_ self: InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.valSubBasic(p, len)
}

func call_valLifetimeboundVirtual(_ self: InheritValue, _ p: UnsafePointer<CInt>!, _ len: CInt) -> UnsafePointer<CInt>! {
  return unsafe self.valLifetimeboundVirtual(p, len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_valLifetimeboundVirtual(_ self: InheritValue, _ p: UnsafeBufferPointer<CInt>) -> UnsafeBufferPointer<CInt> {
  return unsafe self.valLifetimeboundVirtual(p)
}

// FIXME: safe wrappers are cloned but not available?
func call_valNoescape(_ self: InheritValue, _ p: Span<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return self.valNoescape(p)
}
func call_valNonconstSelf(_ self: inout InheritValue, _ p: UnsafeBufferPointer<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.valNonconstSelf(p)
}

func call_valBasic(_ self: InheritValue, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.valBasic(p)
}

func call_valBasicVirt(_ self: InheritValue, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.valBasicVirt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_valSubBasic(_ self: InheritValue, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.valSubBasic(p)
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

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_refNoescape(_ self: RefType, _ p: Span<CInt>) {
  return self.refNoescape(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refBasic(_ self: RefType, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.refBasic(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refNonconstSelf(_ self: RefType, _ p: UnsafeBufferPointer<CInt>) {
  return unsafe self.refNonconstSelf(p)
}

func call_refLifetimeboundVirtual(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) -> UnsafePointer<CInt>! {
  return unsafe self.refLifetimeboundVirtual(p, len)
}

func call_refBasicVirt(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasicVirt(p, len)
}

func call_refBasic(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasic(p, len)
}

func call_refNoescape(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNoescape(p, len)
}

func call_refNonconstSelf(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNonconstSelf(p, len)
}

func call_refLifetimebound(_ self: InheritRef, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.refLifetimebound(p, len)
}
func call_refLifetimeboundSelf(_ self: InheritRef, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.refLifetimeboundSelf(len)
}

func call_refSubBasic(_ self: InheritRef, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refSubBasic(p, len)
}

func call_refNoescape(_ self: InheritRef, _ p: Span<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return self.refNoescape(p)
}

func call_refBasic(_ self: InheritRef, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.refBasic(p)
}

func call_refNonconstSelf(_ self: InheritRef, _ p: UnsafeBufferPointer<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.refNonconstSelf(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refSubBasic(_ self: InheritRef, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.refSubBasic(p)
}

func call_refLifetimeboundVirtual(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) -> UnsafePointer<CInt>! {
  return unsafe self.refLifetimeboundVirtual(p, len)
}

func call_refBasicVirt(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasicVirt(p, len)
}

func call_refBasic(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refBasic(p, len)
}

func call_refNoescape(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNoescape(p, len)
}

func call_refNonconstSelf(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) {
  return unsafe self.refNonconstSelf(p, len)
}

func call_refLifetimebound(_ self: InheritRefPrivate, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.refLifetimebound(p, len)
}
func call_refLifetimeboundSelf(_ self: InheritRefPrivate, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  return unsafe self.refLifetimeboundSelf(len)
}

func call_refPrivateSubBasic(_ self: InheritRefPrivate, _ p: UnsafePointer<CInt>!, _ len: CInt) -> CInt {
  return unsafe self.refPrivateSubBasic(p, len)
}

func call_refNoescape(_ self: InheritRefPrivate, _ p: Span<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return self.refNoescape(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_refPrivateSubBasic(_ self: InheritRefPrivate, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  return unsafe self.refPrivateSubBasic(p)
}

func call_refBasic(_ self: InheritRefPrivate, _ p: UnsafeBufferPointer<CInt>) -> CInt {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.refBasic(p)
}

func call_refNonconstSelf(_ self: InheritRefPrivate, _ p: UnsafeBufferPointer<CInt>) {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  return unsafe self.refNonconstSelf(p)
}
