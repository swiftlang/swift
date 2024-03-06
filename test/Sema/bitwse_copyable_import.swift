// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                               \
// RUN:     %t/Downstream.swift                              \
// RUN:     -typecheck -verify                               \
// RUN:     -enable-experimental-feature NonescapableTypes   \
// RUN:     -enable-experimental-feature BitwiseCopyable     \
// RUN:     -enable-builtin-module                           \
// RUN:     -debug-diagnostic-names                          \
// RUN:     -import-objc-header %t/Library.h

//--- Library.h

struct Tenple {
  int i0;
  int i1;
  int i2;
  int i3;
  int i4;
  int i5;
  int i6;
  int i7;
  int i8;
  int i9;
};

struct Ints128 {
  int is[128];
};

struct VoidPointers {
  void *p1;
  void *p2;
  void *p3;
  void *p4;
  void *p5;
  void *p6;
  void *p7;
  void *p8;
  void *p9;
  void *p10;
};

//--- Downstream.swift

func take<T : _BitwiseCopyable>(_ t: T) {}

func passTenple(_ t: Tenple) { take(t) }
func passInts128(_ t: Ints128) {
  take(t)
  take(t.is.0)
  take(t.is.17)
}
func passVoidPointers(_ t: VoidPointers) { 
  take(t) 
  take(t.p10)
}
