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

struct NonFRTStruct {};

// Global/free C++ functions returning non-FRT
NonFRTStruct *_Nonnull global_function_returning_non_FRT();
NonFRTStruct
    *_Nonnull global_function_returning_non_FRT_with_attr_returns_retained()
        __attribute__((swift_attr("returns_retained")));
NonFRTStruct
    *_Nonnull global_function_returning_non_FRT_with_attr_returns_unretained()
        __attribute__((swift_attr("returns_unretained")));
NonFRTStruct *_Nonnull global_function_returning_non_FRT_create();
NonFRTStruct *_Nonnull global_function_returning_non_FRT_copy();

// Struct having static method returning non-FRT
struct StructWithStaticMethodsReturningNonFRT {
  static NonFRTStruct *_Nonnull StaticMethodReturningNonFRT();
  static NonFRTStruct
      *_Nonnull StaticMethodReturningNonFRTWithAttrReturnsRetained()
          __attribute__((swift_attr("returns_retained")));
  static NonFRTStruct
      *_Nonnull StaticMethodReturningNonFRTWithAttrReturnsUnretained()
          __attribute__((swift_attr("returns_unretained")));
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
