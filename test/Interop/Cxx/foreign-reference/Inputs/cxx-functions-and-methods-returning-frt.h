#pragma once

// FRT or SWIFT_SHARED_REFERENCE type
struct FRTStruct {
  // Friend function declarations
  friend FRTStruct *returnInstanceOfFRTStruct(int v);
  friend FRTStruct *returnInstanceOfFRTStructWithAttrReturnsRetained(int v);
  friend FRTStruct *returnInstanceOfFRTStructWithAttrReturnsUnretained(int v);
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainFRTStruct")))
__attribute__((swift_attr("release:releaseFRTStruct")));

// Retain function for the FRT or SWIFT_SHARED_REFERENCE type FRTStruct
void retainFRTStruct(FRTStruct *_Nonnull b) {}

// Release function for the FRT or SWIFT_SHARED_REFERENCE type FRTStruct
void releaseFRTStruct(FRTStruct *_Nonnull b) {}

// Friend function definition
FRTStruct *returnInstanceOfFRTStruct() { return new FRTStruct; }

// Friend function definition
FRTStruct *returnInstanceOfFRTStructWithAttrReturnsRetained()
    __attribute__((swift_attr("returns_retained"))) {
  return new FRTStruct;
}

// Friend function definition
FRTStruct *returnInstanceOfFRTStructWithAttrReturnsUnretained()
    __attribute__((swift_attr("returns_unretained"))) {
  return new FRTStruct;
}

// Global/free C++ functions returning FRT without any attributes
FRTStruct *_Nonnull global_function_returning_FRT();
FRTStruct *_Nonnull global_function_returning_copy();
FRTStruct *_Nonnull global_function_returning_create();
FRTStruct *_Nonnull global_function_returning_init();
FRTStruct *_Nonnull global_function_returning_clone();

// Global/free C++ functions returning FRT with attribute
// swift_attr("returns_retained") or SWIFT_RETURNS_RETAINED
FRTStruct *_Nonnull global_function_returning_FRT_with_attr_returns_retained()
    __attribute__((swift_attr("returns_retained")));
FRTStruct *_Nonnull global_function_returning_copy_with_attr_returns_retained()
    __attribute__((swift_attr("returns_retained")));
FRTStruct
    *_Nonnull global_function_returning_create_with_attr_returns_retained()
        __attribute__((swift_attr("returns_retained")));
FRTStruct *_Nonnull global_function_returning_init_with_attr_returns_retained()
    __attribute__((swift_attr("returns_retained")));
FRTStruct *_Nonnull global_function_returning_clone_with_attr_returns_retained()
    __attribute__((swift_attr("returns_retained")));

// Global/free C++ functions returning FRT with attribute
// swift_attr("returns_unretained") or SWIFT_RETURNS_UNRETAINED
FRTStruct *_Nonnull global_function_returning_FRT_with_attr_returns_unretained()
    __attribute__((swift_attr("returns_unretained")));
FRTStruct
    *_Nonnull global_function_returning_copy_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));
FRTStruct
    *_Nonnull global_function_returning_create_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));
FRTStruct
    *_Nonnull global_function_returning_init_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));
FRTStruct
    *_Nonnull global_function_returning_clone_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));

// Static Global/free functions returning FRT
static FRTStruct *_Nonnull global_static_function_returning_FRT();
static FRTStruct
    *_Nonnull global_static_function_returning_FRT_with_attr_returns_retained()
        __attribute__((swift_attr("returns_retained")));
static FRTStruct
    *_Nonnull global_static_function_returning_FRT_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));
static FRTStruct *_Nonnull global_static_function_returning_copy();
static FRTStruct *_Nonnull global_static_function_returning_create();
static FRTStruct
    *_Nonnull global_static_function_returning_copy_with_attr_returns_retained()
        __attribute__((swift_attr("returns_retained")));
static FRTStruct
    *_Nonnull global_static_function_returning_copy_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));

// Global/free functions returning FRT without _Nonnull
FRTStruct *global_function_returning_FRT_wihout_Nonnull();
FRTStruct *
global_function_returning_FRT_with_attr_returns_retained_wihout_Nonnull()
    __attribute__((swift_attr("returns_retained")));
FRTStruct *
global_function_returning_FRT_with_attr_returns_unretained_wihout_Nonnull()
    __attribute__((swift_attr("returns_unretained")));
FRTStruct *global_function_returning_copy_wihout_Nonnull();
FRTStruct *global_function_returning_create_wihout_Nonnull();
FRTStruct *
global_function_returning_copy_with_attr_returns_retained_wihout_Nonnull()
    __attribute__((swift_attr("returns_retained")));
FRTStruct *
global_function_returning_copy_with_attr_returns_unretained_wihout_Nonnull()
    __attribute__((swift_attr("returns_unretained")));

// Struct having static methods returning FRT without any attributes
struct StructWithStaticMethodsReturningFRTWithoutAttributes {
  static FRTStruct *_Nonnull StaticMethodReturningFRT();
  static FRTStruct *_Nonnull StaticMethodReturningFRT_copy();
  static FRTStruct *_Nonnull StaticMethodReturningFRT_create();
  static FRTStruct *_Nonnull StaticMethodReturningFRT_init();
  static FRTStruct *_Nonnull StaticMethodReturningFRT_clone();
};

// Struct having static methods returning FRT with attribute
// swift_attr("returns_retained") or SWIFT_RETURNS_RETAINED
struct StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained {
  static FRTStruct *_Nonnull StaticMethodReturningFRT()
      __attribute__((swift_attr("returns_retained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_copy()
      __attribute__((swift_attr("returns_retained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_create()
      __attribute__((swift_attr("returns_retained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_init()
      __attribute__((swift_attr("returns_retained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_clone()
      __attribute__((swift_attr("returns_retained")));
};

// Struct having static methods returning FRT with attribute
// swift_attr("returns_unretained") or SWIFT_RETURNS_UNRETAINED
struct StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained {
  static FRTStruct *_Nonnull StaticMethodReturningFRT()
      __attribute__((swift_attr("returns_unretained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_copy()
      __attribute__((swift_attr("returns_unretained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_create()
      __attribute__((swift_attr("returns_unretained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_init()
      __attribute__((swift_attr("returns_unretained")));
  static FRTStruct *_Nonnull StaticMethodReturningFRT_clone()
      __attribute__((swift_attr("returns_unretained")));
};

// Global/free C++ functions returning FRT with both attributes
// swift_attr("returns_unretained") and swift_attr("returns_retained")
FRTStruct
    *_Nonnull global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
        __attribute__((swift_attr("returns_retained")))
        __attribute__((swift_attr("returns_unretained")));

// Struct having static method returning FRT with both attributes
// swift_attr("returns_unretained") and swift_attr("returns_retained")
struct
    StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained {
  static FRTStruct *_Nonnull StaticMethodReturningFRT()
      __attribute__((swift_attr("returns_retained")))
      __attribute__((swift_attr("returns_unretained")));
};

// A c++ struct not annotated with SWIFT_SHARED_REFERENCE,
// SWIFT_IMMORTAL_REFERENCE or SWIFT_UNSAFE_REFERENCE
struct NonFRTStruct {};

// A c++ struct annotated with SWIFT_IMMORTAL_REFERENCE
struct ImmortalRefStruct {
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

// A c++ struct annotated with SWIFT_UNSAFE_REFERENCE
struct UnsafeRefStruct {
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
__attribute__((swift_attr("unsafe")));

// C++ APIs returning cxx frts (for testing diagnostics)
struct StructWithAPIsReturningCxxFrt {
  static FRTStruct *_Nonnull StaticMethodReturningCxxFrt();
  static FRTStruct *_Nonnull StaticMethodReturningCxxFrtWithAnnotation()
      __attribute__((swift_attr("returns_retained")));
};

FRTStruct *_Nonnull global_function_returning_cxx_frt();
FRTStruct *_Nonnull global_function_returning_cxx_frt_with_annotations()
    __attribute__((swift_attr("returns_retained")));

// C++ APIs returning non-cxx-frts (for testing diagnostics)
struct StructWithAPIsReturningNonCxxFrt {
  static NonFRTStruct *_Nonnull StaticMethodReturningNonCxxFrt();
  static NonFRTStruct *_Nonnull StaticMethodReturningNonCxxFrtWithAnnotation()
      __attribute__((swift_attr("returns_retained")));
};

NonFRTStruct *_Nonnull global_function_returning_non_cxx_frt();
NonFRTStruct *_Nonnull global_function_returning_non_cxx_frt_with_annotations()
    __attribute__((swift_attr("returns_retained")));

// C++ APIs returning SWIFT_IMMORTAL_REFERENCE types (for testing diagnostics)
struct StructWithAPIsReturningImmortalReference {
  static ImmortalRefStruct *_Nonnull StaticMethodReturningImmortalReference();
  static ImmortalRefStruct
      *_Nonnull StaticMethodReturningImmortalReferenceWithAnnotation()
          __attribute__((swift_attr("returns_retained")));
};

ImmortalRefStruct *_Nonnull global_function_returning_immortal_reference();
ImmortalRefStruct
    *_Nonnull global_function_returning_immortal_reference_with_annotations()
        __attribute__((swift_attr("returns_retained")));

// C++ APIs returning SWIFT_UNSAFE_REFERENCE types (for testing diagnostics)
struct StructWithAPIsReturningUnsafeReference {
  static UnsafeRefStruct *_Nonnull StaticMethodReturningUnsafeReference();
  static UnsafeRefStruct
      *_Nonnull StaticMethodReturningUnsafeReferenceWithAnnotation()
          __attribute__((swift_attr("returns_retained")));
};

UnsafeRefStruct *_Nonnull global_function_returning_unsafe_reference();
UnsafeRefStruct
    *_Nonnull global_function_returning_unsafe_reference_with_annotations()
        __attribute__((swift_attr("returns_retained")));

// Global/free C++ functions returning non-FRT
NonFRTStruct *_Nonnull global_function_returning_non_FRT();
NonFRTStruct *_Nonnull global_function_returning_non_FRT_create();
NonFRTStruct *_Nonnull global_function_returning_non_FRT_copy();

// Struct having static method returning non-FRT
struct StructWithStaticMethodsReturningNonFRT {
  static NonFRTStruct *_Nonnull StaticMethodReturningNonFRT();
  static NonFRTStruct *_Nonnull StaticMethodReturningNonFRT_create();
  static NonFRTStruct *_Nonnull StaticMethodReturningNonFRT_copy();
};

// Tests for templated functions
template <typename T>
FRTStruct *_Nonnull global_templated_function_returning_FRT(T a);

template <typename T>
FRTStruct *_Nonnull global_templated_function_returning_FRT_copy(T a);

template <typename T>
FRTStruct *_Nonnull global_templated_function_returning_FRT_create(T a);

template <typename T>
FRTStruct *_Nonnull global_templated_function_returning_FRT_init(T a);

template <typename T>
FRTStruct *_Nonnull global_templated_function_returning_FRT_clone(T a);

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_with_attr_returns_retained(
        T a) __attribute__((swift_attr("returns_retained")));

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_copy_with_attr_returns_retained(
        T a) __attribute__((swift_attr("returns_retained")));

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_create_with_attr_returns_retained(
        T a) __attribute__((swift_attr("returns_retained")));

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_with_attr_returns_unretained(
        T a) __attribute__((swift_attr("returns_unretained")));

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_copy_with_attr_returns_unretained(
        T a) __attribute__((swift_attr("returns_unretained")));

template <typename T>
FRTStruct
    *_Nonnull global_templated_function_returning_FRT_create_with_attr_returns_unretained(
        T a) __attribute__((swift_attr("returns_unretained")));
