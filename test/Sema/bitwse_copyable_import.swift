// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                               \
// RUN:     %t/Downstream.swift                              \
// RUN:     -typecheck -verify                               \
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

struct IntsTrailing {
  double d;
  float f;
  int is[];
};

struct IntsTrailing2 {
  double d;
  float f;
  int is[];
};

struct IntsTrailing3 {
  double d;
  float f;
  int is[];
} __attribute__((__swift_attr__("BitwiseCopyable")));

enum E {
  foo,
  bar
};

//--- Downstream.swift

func take<T : BitwiseCopyable>(_ t: T) {}

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
func passIntsTrailing(_ t: IntsTrailing) {
  take(t) // expected-error{{type_does_not_conform_decl_owner}}
          // expected-note@-14{{where_requirement_failure_one_subst}}
}
extension IntsTrailing2 : BitwiseCopyable {} //expected-error{{bitwise_copyable_outside_module}}
func passIntsTrailing2(_ t: IntsTrailing2) {
  take(t)
}
func passIntsTrailing3(_ t: IntsTrailing3) {
  take(t)
}

func passE(_ e: E) {
  take(e)
}
