// RUN: %target-typecheck-verify-swift                       \
// RUN:     -disable-availability-checking                   \
// RUN:     -enable-experimental-feature NonescapableTypes   \
// RUN:     -enable-experimental-feature BitwiseCopyable     \
// RUN:     -enable-builtin-module                           \
// RUN:     -debug-diagnostic-names

// This test file only exists in order to test without noncopyable_generics and can be deleted once that is always enabled.

@_nonescapable
struct S_Implicit_Nonescapable {}

struct S_Implicit_Noncopyable : ~Copyable {}

struct S_Explicit_With_Any_BitwiseCopyable : _BitwiseCopyable {
  var a: any _BitwiseCopyable // expected-error {{non_bitwise_copyable_type_member}}
}

struct S {}

indirect enum E_Explicit_Indirect : _BitwiseCopyable { // expected-error {{non_bitwise_copyable_type_indirect_enum}}
  case s(S)
}

enum E_Explicit_Indirect_Case : _BitwiseCopyable { // expected-error {{non_bitwise_copyable_type_indirect_enum_element}}
  indirect case s(S) // expected-note {{note_non_bitwise_copyable_type_indirect_enum_element}}
}

func take<T : _BitwiseCopyable>(_ t: T) {}

// public (effectively) => !conforms
@usableFromInline internal struct InternalUsableStruct {
  public var int: Int
}

func passInternalUsableStruct(_ s: InternalUsableStruct) { take(s) } // expected-error{{type_does_not_conform_decl_owner}}
                                                                     // expected-note@-8{{where_requirement_failure_one_subst}}
