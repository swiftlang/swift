// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated \
// RUN:     -disable-availability-checking                      \
// RUN:     -enable-builtin-module                              \
// RUN:     -debug-diagnostic-names

// Test conformances for non-stdlib types

func load1<T: ConvertibleFromBytes>(as: T.Type = T.self) -> T { fatalError() }
func store1<T: ConvertibleToBytes>(_ t: T) {}

class C1_Implicit {}

func storeC1_Implicit(_ c: C1_Implicit) { store1(c) } // expected-error {{type_does_not_conform_decl_owner}}
                                                      // expected-note@-5 {{where_requirement_failure_one_subst}}

func loadC1_Implicit() -> C1_Implicit { load1() } // expected-error {{type_does_not_conform_decl_owner}}
                                                  // expected-note@-9 {{where_requirement_failure_one_subst}}

struct A1: ConvertibleToBytes { var a: Int } // expected-error {{convertible_to_bytes_outside_stdlib}}

struct B1 { var a, b: Int }
extension B1: ConvertibleFromBytes {} // expected-error {{convertible_from_bytes_outside_stdlib}}

struct C1 { var a: Int16 }
@available(*, unavailable) extension C1: ConvertibleToBytes {}   // no diagnostic
@available(*, unavailable) extension C1: ConvertibleFromBytes {} // no diagnostic

struct D1 { var a, b: Int }
extension D1: BitwiseCopyable {}
extension D1: @unchecked ConvertibleToBytes {}   // expected-warning {{unchecked_conformance_not_special}}
                                                 // expected-error@-1 {{convertible_to_bytes_outside_stdlib}}
extension D1: @unchecked ConvertibleFromBytes {} // expected-error {{convertible_from_bytes_outside_stdlib}}

//struct E1: ~Copyable { var a, b: Int }
//extension E1: ConvertibleToBytes {}

//struct F1 { var a, b: Int }
//@available(*, unavailable) extension F1: BitwiseCopyable {}
//extension F1: ConvertibleFromBytes {}

// Test conformances of stdlib types

func load2<T: ConvertibleFromBytes>(as: T.Type = T.self) -> T { fatalError() }
func loadUInt64() -> UInt64 { load2() }

func storeUInt64(_ i: UInt64) { store2(i) }
func store2<T: ConvertibleToBytes>(_ t: T) {}

func roundTrip2<T: FullyInhabited>(_ t: T) -> T { store2(t); return load2() }
func roundTripInt32(_ i: Int32) -> Int32 { roundTrip2(i) }

extension ManagedBuffer: @retroactive ConvertibleToBytes {}   // expected-error {{convertible_to_bytes_outside_module}}
extension ManagedBuffer: @retroactive ConvertibleFromBytes {} // expected-error {{convertible_from_bytes_outside_module}}
                                                              // expected-error@-1 {{bitwise_copyable_outside_module}}
