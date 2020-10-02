#ifndef TEST_INTEROP_C_STRUCT_INPUTS_STRUCT_DECL_CONTEXT_H
#define TEST_INTEROP_C_STRUCT_INPUTS_STRUCT_DECL_CONTEXT_H

// This header contains C structs that are declared and nested in various ways.
// Some of these patterns are special-cased by ClangImporter (for example, the
// names of typedefs become names of imported structs). Nested structs have been
// historically imported in Swift according to C rules, that is, structs
// lexically declared nested in other structs are imported into the global
// namespace anyway.

struct StructRegular {
  struct StructNestedComplete1 {
    struct StructNestedNestedComplete1 {} complete_immediately_nested;
    struct StructNestedNestedCompletedLater1 *completed_later_nested;
  } complete_immediately;
  struct StructNestedCompletedLater1 *completed_later;
};

struct StructNestedNestedCompletedLater1 {};
struct StructNestedCompletedLater1 {};

typedef struct StructTypedefTag2 {
  struct StructNestedComplete2 {} complete_immediately;
  struct StructNestedCompletedLater2 *completed_later;
} StructTypedefName2;

struct StructNestedCompletedLater2 {};

typedef struct {
  struct StructNestedComplete3 {} complete_immediately;
  struct StructNestedCompletedLater3 *completed_later;
} StructTypedefName3;

struct StructNestedCompletedLater3 {};

typedef struct StructTypedefTag4 {
  struct StructNestedComplete4 {} complete_immediately;
  struct StructNestedCompletedLater4 *completed_later;
} *StructTypedefName4;

struct StructNestedCompletedLater4 {};

typedef struct {
  struct StructNestedComplete5 {} complete_immediately;
  struct StructNestedCompletedLater5 *completed_later;
} *StructTypedefName5;

struct StructNestedCompletedLater5 {};

typedef struct {
  struct StructNestedComplete6 {} complete_immediately;
  struct StructNestedCompletedLater6 *completed_later;
} StructTypedefName6, *StructTypedefName6Ptr;

struct StructNestedCompletedLater6 {};

typedef struct {
  struct StructNestedComplete7 {} complete_immediately;
  struct StructNestedCompletedLater7 *completed_later;
} *StructTypedefName7Ptr, StructTypedefName7;

struct StructNestedCompletedLater7 {};

typedef struct {
  struct StructNestedComplete8 {} complete_immediately;
  struct StructNestedCompletedLater8 *completed_later;
} StructTypedefName8, *StructTypedefName8Ptr, **StructTypedefName8PtrPtr;

struct StructNestedCompletedLater8 {};

#endif
