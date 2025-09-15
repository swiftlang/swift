// RUN: %target-typecheck-verify-swift                          \
// RUN:     -enable-builtin-module                              \
// RUN:     -enable-experimental-feature LifetimeDependence     \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_LifetimeDependence

// This test file only exists in order to test without noncopyable_generics and can be deleted once that is always enabled.

@_nonescapable
struct S_Implicit_Nonescapable {}

struct S_Implicit_Noncopyable : ~Copyable {}

struct S_Explicit_With_AnyBitwiseCopyable : BitwiseCopyable {
  var a: any BitwiseCopyable // expected-error {{non_bitwise_copyable_type_member}}
}

struct S {}

indirect enum E_Explicit_Indirect : BitwiseCopyable { // expected-error {{non_bitwise_copyable_type_indirect_enum}}
  case s(S)
}

enum E_Explicit_Indirect_Case : BitwiseCopyable { // expected-error {{non_bitwise_copyable_type_indirect_enum_element}}
  indirect case s(S) // expected-note {{note_non_bitwise_copyable_type_indirect_enum_element}}
}

func take<T : BitwiseCopyable>(_ t: T) {}

// public (effectively) => !conforms
@usableFromInline internal struct InternalUsableStruct {
  public var int: Int
}

func passInternalUsableStruct(_ s: InternalUsableStruct) { take(s) } // expected-error{{type_does_not_conform_decl_owner}}
                                                                     // expected-note@-8{{where_requirement_failure_one_subst}}

func passMemoryLayout<T>(_ m: MemoryLayout<T>) { take(m) } // expected-error{{global function 'take' requires that 'MemoryLayout<T>' conform to 'BitwiseCopyable'}}
                                                           // expected-note@-11{{where 'T' = 'MemoryLayout<T>'}}

func passCommandLine(_ m: CommandLine) { take(m) } // expected-error{{global function 'take' requires that 'CommandLine' conform to 'BitwiseCopyable'}}
                                                   // expected-note@-14{{where 'T' = 'CommandLine'}}

func passUnicode(_ m: Unicode) { take(m) } // expected-error{{global function 'take' requires that 'Unicode' conform to 'BitwiseCopyable'}}
                                           // expected-note@-17{{where 'T' = 'Unicode'}}

import Builtin

#if arch(arm)
func passBuiltinFloat16(_ f: Builtin.FPIEEE16) { take(f) }
@available(SwiftStdlib 5.3, *)
func passFloat16(_ f: Float16) { take(f) }
#endif

enum E_Raw_Int : Int {
  case one = 1
  case sixty_three = 63
}

func passE_Raw_Int(_ e: E_Raw_Int) { take(e) }

enum E_Raw_String : String {
  case one = "one"
  case sixty_three = "sixty three"
}

func passE_Raw_String(_ e: E_Raw_String) { take(e) }

struct S_Suppressed : ~BitwiseCopyable {
  var i: Int
}

func passS_Suppressed(_ s: S_Suppressed) { take(s) } // expected-error{{global function 'take' requires that 'S_Suppressed' conform to 'BitwiseCopyable'}}
                                                     // expected-note@-46{{where 'T' = 'S_Suppressed'}}

struct S_Suppressed_Extension {}
extension S_Suppressed_Extension : ~BitwiseCopyable {} // expected-error{{conformance to inferrable protocol 'BitwiseCopyable' cannot be suppressed in an extension}}

struct S_Explicit_Suppressed : BitwiseCopyable, ~BitwiseCopyable {} // expected-error{{cannot both conform to and suppress conformance to 'BitwiseCopyable'}}

func passS_Explicit_Suppressed(_ s: S_Explicit_Suppressed) { take(s) }

struct S_Suppressed_Explicit : ~BitwiseCopyable, BitwiseCopyable {} // expected-error{{cannot both conform to and suppress conformance to 'BitwiseCopyable'}}

func passS_Explicit_Suppressed(_ s: S_Suppressed_Explicit) { take(s) }

struct S_Suppressed_Twice : ~BitwiseCopyable, ~BitwiseCopyable {} // expected-warning{{already suppressed conformance to 'BitwiseCopyable'}}

enum E : ~Equatable { // expected-error{{conformance to 'Equatable' cannot be suppressed}}
  case i
  case e
  case io
}

enum E_Suppressed : ~BitwiseCopyable {}

extension E_Suppressed : BitwiseCopyable {} // expected-error{{cannot both conform to and suppress conformance to 'BitwiseCopyable'}}
