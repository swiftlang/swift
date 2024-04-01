// RUN: %target-typecheck-verify-swift                       \
// RUN:     -disable-availability-checking                   \
// RUN:     -enable-experimental-feature NonescapableTypes   \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     -enable-experimental-feature BitwiseCopyable     \
// RUN:     -enable-builtin-module                           \
// RUN:     -debug-diagnostic-names

// XFAIL: *

//==============================================================================
//===========================DEPENDENCY-FREE TESTS=(BEGIN)===================={{
//==============================================================================

func take1<T : _BitwiseCopyable>(_ t: T) {}

class C_Implicit {}

func passC_Implicit(_ c: C_Implicit) { take1(c) } // expected-error   {{type_does_not_conform_decl_owner}}
                                                  // expected-note@-5 {{where_requirement_failure_one_subst}}

class C_Explicit : _BitwiseCopyable {} // expected-error {{non_bitwise_copyable_type_class}}

func passC_Explicit(_ c: C_Explicit) { take1(c) }

struct S_Empty : _BitwiseCopyable {}

func passS_Empty(_ s: S_Empty) { take1(s) }

struct S_Implicit_With_C_Implicit {
  var guts: C_Implicit
}

func passS_Implicit_With_C_Implicit(_ s: S_Implicit_With_C_Implicit) { 
  take1(s) // expected-error    {{type_does_not_conform_decl_owner}}
           // expected-note@-21 {{where_requirement_failure_one_subst}}
}

struct S_Explicit_With_C_Implicit : _BitwiseCopyable {
  var guts: C_Implicit // expected-error {{non_bitwise_copyable_type_member}}
                       // expected-note@-24 {{non_bitwise_copyable_nominal}}
}

struct S_Explicit_With_Function_Swift : _BitwiseCopyable {
  var f: @convention(swift) () -> () // expected-error   {{non_bitwise_copyable_type_member}}
                                     // expected-note@-1 {{non_bitwise_copyable_function_type}}
}

struct S_Explicit_With_Function_Block : _BitwiseCopyable {
  var f: @convention(block) () -> () // expected-error   {{non_bitwise_copyable_type_member}}
                                     // expected-note@-1 {{non_bitwise_copyable_function_type}}
}

struct S_Explicit_With_Function_Thin : _BitwiseCopyable {
  var f: @convention(thin) () -> ()
}

struct S_Explicit_With_Function_C : _BitwiseCopyable {
  var f: @convention(c) () -> ()
}

public struct S_Public {}

struct S_Explicit_With_S_Public : _BitwiseCopyable {
  var s: S_Public // expected-error   {{non_bitwise_copyable_type_member}}
                  // expected-note@-4 {{add_nominal_bitwise_copyable_conformance}}
}

struct S_Explicit_With_Generic<T> : _BitwiseCopyable {
  var t: T // expected-error   {{non_bitwise_copyable_type_member}}
           // expected-note@-2 {{add_generic_parameter_non_bitwise_copyable_conformance}}
}

protocol Derived : _BitwiseCopyable {}

class C_Explicit_Derived : Derived {} // expected-error {{non_bitwise_copyable_type_class}}

struct S_Explicit_Derived_With_C_Implicit : Derived {
  var c: C_Implicit // expected-error   {{non_bitwise_copyable_type_member}}
                    // expected-note@-63 {{non_bitwise_copyable_nominal}}
}

struct S_Explicit_With_Metatype_S_Public : _BitwiseCopyable {
  var ty: S_Public.Type
}

struct S_Explicit_With_Metatype_Generic<T> : _BitwiseCopyable {
  var ty: T.Type
}

struct S_Explicit_With_Metatype_Optional_Generic<T> : _BitwiseCopyable {
  var ty: Optional<T>.Type
}

struct S_Explicit_With_Metatype_Optional_AnyObject : _BitwiseCopyable {
  var ty: Optional<AnyObject>.Type
}

//==============================================================================
//===========================DEPENDENCY-FREE TESTS=(END)======================}}
//==============================================================================

//==============================================================================
//=========================BUILTIN-DEPENDENCY TESTS=(BEGIN)==================={{
//==============================================================================

import Builtin

func take5<T : _BitwiseCopyable>(_ t: T) {}

// Conforming builtins
func passBuiltinInteger(_ b: Builtin.Int64) { take5(b) }
func passBuiltinFloat(_ b: Builtin.FPIEEE64) { take5(b) }
func passBuiltinPackIndex(_ b: Builtin.PackIndex) { take5(b) }
func passBuiltinRawPointer(_ b: Builtin.RawPointer) { take5(b) }
func passBuiltinVector(_ b: Builtin.Vec2xInt65) { take5(b) }
func passBuiltinExecutor(_ b: Builtin.Executor) { take5(b) }
func passBuiltinJob(_ b: Builtin.Job) { take5(b) }
func passBuiltinRawUnsafeContinuation(_ b: Builtin.RawUnsafeContinuation) { take5(b) }

// Non-conforming builtins
func passBuiltinNativeObject(_ b: Builtin.NativeObject) { take5(b) } // expected-error {{type_does_not_conform_decl_owner}}
                                          // expected-note@-14 {{where_requirement_failure_one_subst}}
func passBuiltinBridgeObject(_ b: Builtin.BridgeObject) { take5(b) } // expected-error {{type_does_not_conform_decl_owner}}
                                          // expected-note@-16 {{where_requirement_failure_one_subst}}
func passBuiltinUnsafeValueBuffer(_ b: Builtin.UnsafeValueBuffer) { take5(b) } // expected-error {{type_does_not_conform_decl_owner}}
                                          // expected-note@-18 {{where_requirement_failure_one_subst}}
func passBuiltinDefaultActorStorage(_ b: Builtin.DefaultActorStorage) { take5(b) } // expected-error {{type_does_not_conform_decl_owner}}
                                          // expected-note@-20 {{where_requirement_failure_one_subst}}
func passBuiltinNonDefaultDistributedActorStorage(_ b: Builtin.NonDefaultDistributedActorStorage) { take5(b) } // expected-error {{type_does_not_conform_decl_owner}}
                                          // expected-note@-22 {{where_requirement_failure_one_subst}}


//==============================================================================
//==========================BUILTIN-DEPENDENCY TESTS=(END)====================}}
//==============================================================================

//==============================================================================
//==========================STDLIB-DEPENDENCY TESTS=(BEGIN)==================={{
//==============================================================================

func take3<T : _BitwiseCopyable>(_ t: T) {}

func passInt(_ i: Int) { take3(i) }

func passTupleIntInt(_ t: (Int, Int)) { take3(t) }

func passFWI<T : FixedWidthInteger>(_ t: T) { take3(t) }

func passFloat(_ f: Float) { take3(f) }

func passAnyObject(_ o: AnyObject) { take3(o) } // expected-error {{type_does_not_conform_decl_owner}}

func passAnyAnyObject(_ o: any AnyObject) { take3(o) } // expected-error {{type_does_not_conform_decl_owner}}

func passAny(_ a: Any) { take3(a) } // expected-error {{type_does_not_conform_decl_owner}}

func passAnyAny(_ a: any Any) { take3(a) } // expected-error {{type_does_not_conform_decl_owner}}

func passString(_ s: String) { take3(s) } // expected-error    {{type_does_not_conform_decl_owner}}
                                          // expected-note@-19 {{where_requirement_failure_one_subst}}

extension Optional where Wrapped : Copyable & Escapable {
  struct Some : _BitwiseCopyable & Copyable & Escapable {
    var wrapped: Wrapped // expected-error {{non_bitwise_copyable_type_member}}
  }
}

extension Optional where Wrapped : _BitwiseCopyable {
  struct Some2 : _BitwiseCopyable {
    var wrapped: Wrapped
  }
}

struct S_Explicit_With_Generic_Optional<T> : _BitwiseCopyable {
  var o: Optional<T> // expected-error {{non_bitwise_copyable_type_member}}
}

struct S_Explicit_With_2_Generic_Optionals<T> : _BitwiseCopyable {
  var o1: Optional<T> // expected-error {{non_bitwise_copyable_type_member}}
  var o2: T? // expected-error {{non_bitwise_copyable_type_member}}
}

struct S_Explicit_With_BitwiseCopyable_Generic_Optional<T : _BitwiseCopyable> : _BitwiseCopyable {
  var o: Optional<T>
}

struct S_Explicit_With_2_BitwiseCopyable_Generic_Optional<T : _BitwiseCopyable> : _BitwiseCopyable {
  var o: Optional<T>
  var o2: T?
}

struct S_Explicit_Nonescapable : ~Escapable, _BitwiseCopyable {} // expected-error{{non_bitwise_copyable_type_nonescapable}}

struct S_Explicit_Noncopyable : ~Copyable, _BitwiseCopyable {} // expected-error{{non_bitwise_copyable_type_noncopyable}}

struct S_Implicit_Nonescapable : ~Escapable {}

struct S_Implicit_Noncopyable : ~Copyable {}


func passUnmanaged<T : AnyObject>(_ u: Unmanaged<T>) { take3(u) }

struct S_Explicit_With_Unmanaged<T : AnyObject> : _BitwiseCopyable {
  var u: Unmanaged<T>
}

func passUnsafePointer<T>(_ p: UnsafePointer<T>) { take3(p) }

struct S_Explicit_With_UnsafePointer<T> : _BitwiseCopyable  {
  var ptr: UnsafePointer<T>
}

func passUnsafeMutablePointer<T>(_ p: UnsafeMutablePointer<T>) { take3(p) }

struct S_Explicit_With_UnsafeMutablePointer<T> : _BitwiseCopyable  {
  var ptr: UnsafeMutablePointer<T>
}

func passUnsafeBufferPointer<T>(_ p: UnsafeBufferPointer<T>) { take3(p) }

struct S_Explicit_With_UnsafeBufferPointer<T> : _BitwiseCopyable  {
  var ptr: UnsafeBufferPointer<T>
}

func passUnsafeMutableBufferPointer<T>(_ p: UnsafeMutableBufferPointer<T>) { take3(p) }

struct S_Explicit_With_UnsafeMutableBufferPointer<T> : _BitwiseCopyable  {
  var ptr: UnsafeMutableBufferPointer<T>
}

func passPointer<T : _Pointer>(_ p: T) { take3(p) }

//==============================================================================
//==========================STDLIB-DEPENDENCY TESTS=(END)=====================}}
//==============================================================================

//==============================================================================
//=======================_CONCURRENCY-DEPENDENCY TESTS=(BEGIN)================{{
//==============================================================================

func take4<T : _BitwiseCopyable>(_ t: T) {}

func passCheckedContinuation<T : _BitwiseCopyable, U : Error & _BitwiseCopyable>(
  _ c: CheckedContinuation<T, U>
)
{
  take4(c) // expected-error   {{type_does_not_conform_decl_owner}}
           // expected-note@-7 {{where_requirement_failure_one_subst}}
}

func passUnsafeContinuation<T : _BitwiseCopyable, U : Error & _BitwiseCopyable>(
  _ c: UnsafeContinuation<T, U>
)
{
  take4(c)
}

extension UnsafeContinuation : @retroactive _BitwiseCopyable {}

//==============================================================================
//========================_CONCURRENCY-DEPENDENCY TESTS=(END)=================}}
//==============================================================================
