// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift %t/main.swift -import-objc-header %t/header.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %t/main.swift
// RUN: %target-build-swift %t/main.swift -import-objc-header %t/header.h -o %t/a-td.out \
// RUN:   -Xcc -Xclang -Xcc -fptrauth-function-pointer-type-discrimination
// RUN: %target-codesign %t/a-td.out
// RUN: %target-run %t/a-td.out | %FileCheck %t/main.swift

// REQUIRES: executable_test
// REQUIRES: CPU=arm64e

//--- header.h

#include <ptrauth.h>
#include <stdarg.h>

struct node {
  struct node *_Nullable __ptrauth(ptrauth_key_process_independent_data, 1,
                                   ptrauth_string_discriminator("node.parent")) parent;
};

static inline struct node *_Nullable c_get_parent(struct node *_Nonnull n) {
  return n->parent;
}
static inline void c_set_parent(struct node *_Nonnull n, struct node *_Nullable p) {
  n->parent = p;
}

// A variadic function pointer is imported as OpaquePointer.
static inline int sum(int n, ...) {
  va_list ap;
  va_start(ap, n);
  int s = 0;
  while (n--)
    s += va_arg(ap, int);
  va_end(ap);
  return s;
}

struct vbox {
  int (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 31) fn)(int, ...);
};

static inline struct vbox make_vbox(void) {
  struct vbox b;
  b.fn = sum;
  return b;
}
static inline int (*_Nonnull get_sum(void))(int, ...) { return sum; }
static inline int call_fn(int (*_Nonnull f)(int, ...)) { return f(2, 20, 22); }
static inline int call_field(const struct vbox *_Nonnull b) { return b->fn(2, 20, 22); }

//--- main.swift

let a = UnsafeMutablePointer<node>.allocate(capacity: 2)
let b = a + 1

// CHECK: C store, Swift load: true
c_set_parent(a, b)
print("C store, Swift load: \(a.pointee.parent == b)")

// CHECK: Swift store, C load: true
a.pointee.parent = b
print("Swift store, C load: \(c_get_parent(a) == b)")

// CHECK: nil: true true
a.pointee.parent = nil
let swiftStoredNil = c_get_parent(a) == nil
c_set_parent(a, nil)
print("nil: \(swiftStoredNil) \(a.pointee.parent == nil)")
a.deallocate()

// CHECK: variadic read: 42
let v = make_vbox()
print("variadic read: \(call_fn(v.fn!))")

// CHECK: variadic write: 42
var w = vbox()
w.fn = get_sum()
print("variadic write: \(call_field(&w))")
