// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name RawIdentifiers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/raw.h
// RUN: %FileCheck %s < %t/raw.h

// RUN: %check-interop-cxx-header-in-clang(%t/raw.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// Swift names that are not valid C++ identifiers are sanitized when they are
// exposed to C++: every character that is not valid in a C++ identifier is
// replaced with its Unicode scalar value, spelled like a C++
// universal-character-name (`_uXXXX` or `_UXXXXXXXX`), and a leading digit is
// preceded by an underscore. A declaration with a sanitized name gets a doc
// comment that states its original Swift name.

// CHECK: namespace RawIdentifiers SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("RawIdentifiers") {

public enum `Enum Name` {
  case `default`
  case `1`
  case `2`(CInt)
  case `hello world`
}
// CHECK: /// Swift name: '`Enum Name`'
// CHECK-NEXT: class SWIFT_SYMBOL({{.*}}) Enum_u0020Name final {
// CHECK: enum class cases {
// CHECK-NEXT: /// Swift name: '`2`(_:)'
// CHECK-NEXT: _2 SWIFT_SYMBOL({{.*}}),
// CHECK-NEXT: default_ SWIFT_SYMBOL({{.*}}),
// CHECK-NEXT: /// Swift name: '`1`'
// CHECK-NEXT: _1 SWIFT_SYMBOL({{.*}}),
// CHECK-NEXT: /// Swift name: '`hello world`'
// CHECK-NEXT: hello_u0020world SWIFT_SYMBOL({{.*}})
// CHECK-NEXT: };
// CHECK: inline const static struct _impl__2 {  // impl struct for case _2
// CHECK: constexpr operator cases() const {
// CHECK-NEXT: return cases::_2;
// CHECK: } _2 SWIFT_SYMBOL({{.*}});
// CHECK: bool is_2() const;
// CHECK: int get_2() const;
// CHECK: inline const static struct _impl_default {  // impl struct for case default
// CHECK: constexpr operator cases() const {
// CHECK-NEXT: return cases::default_;
// CHECK: } default_ SWIFT_SYMBOL({{.*}});
// CHECK: bool isDefault_() const;
// CHECK: inline const static struct _impl__1 {  // impl struct for case _1
// CHECK: constexpr operator cases() const {
// CHECK-NEXT: return cases::_1;
// CHECK: } _1 SWIFT_SYMBOL({{.*}});
// CHECK: bool is_1() const;
// CHECK: inline const static struct _impl_hello_u0020world {  // impl struct for case hello_u0020world
// CHECK: constexpr operator cases() const {
// CHECK-NEXT: return cases::hello_u0020world;
// CHECK: } hello_u0020world SWIFT_SYMBOL({{.*}});
// CHECK: bool isHello_u0020world() const;

public struct `Struct Name` {
  public var `prop name`: CInt

  public init(_ x: CInt) {
    self.`prop name` = x
  }

  public func `method name`() -> CInt {
    return `prop name` * 2
  }
}
// CHECK: /// Swift name: '`Struct Name`'
// CHECK-NEXT: class SWIFT_SYMBOL({{.*}}) Struct_u0020Name final {
// CHECK: /// Swift name: '`prop name`'
// CHECK-NEXT: SWIFT_INLINE_THUNK int getProp_u0020name() const
// CHECK: /// Swift name: '`prop name`'
// CHECK-NEXT: SWIFT_INLINE_THUNK void setProp_u0020name(int
// CHECK: /// Swift name: '`method name`()'
// CHECK-NEXT: SWIFT_INLINE_THUNK int method_u0020name() const

public func `hello world`() -> CInt {
  return 42
}
// CHECK: /// Swift name: '`hello world`()'
// CHECK-NEXT: SWIFT_INLINE_THUNK int hello_u0020world() noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {

public func über(_ `param name`: CInt) -> CInt {
  return `param name` + 1
}
// CHECK: /// Swift name: 'über(_:)'
// CHECK-NEXT: SWIFT_INLINE_THUNK int _u00FCber(int param_u0020name) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {

public func 🚀speed() -> CInt {
  return 100
}
// CHECK: /// Swift name: '🚀speed()'
// CHECK-NEXT: SWIFT_INLINE_THUNK int _U0001F680speed() noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {

// A Swift operator function whose spelling is not a valid C++ operator is not
// exposed, instead of emitting a nameless C++ function that would not compile.
infix operator +++
public func +++ (a: CInt, b: CInt) -> CInt {
  return a + b
}
// CHECK: // Unavailable in C++: Swift operator function '+++(_:_:)'. the operator can not be represented as a C++ operator.
