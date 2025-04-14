// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swiftxx-frontend %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil | %FileCheck %s

//--- test.h

const int const_int = 42;
namespace Namespace {
  const int namespaced_const_int = 42;
}

static inline int foo() { return 42; }
const int not_really_constant = foo();

constexpr int constexpr_func() { return 42; }
constexpr int constexpr_int = constexpr_func();
static constexpr int static_constexpr_int = constexpr_func();

class MyClass {
public:
  const int class_const_int = 42; // member field, don't expect to import as a global, no CHECKs for this
  static const int class_static_const_int = 42;
};

namespace OtherNamespace {
  constexpr int namespaced_constexpr_int = constexpr_func();
  static constexpr int namespaced_static_constexpr_int = constexpr_func();
}
class OtherClass {
  static constexpr int class_static_constexpr_int = 42;
};

template <int N, int M>
inline const int template_gcd = template_gcd<M, N % M>;

template <int N>
inline const int template_gcd<N, 0> = N;


//--- main.swift
func foo() {
  print(const_int)
  print(Namespace.namespaced_const_int)
  print(not_really_constant)
  print(constexpr_int)
  print(static_constexpr_int)

  // TODO: Non-top-level constexpr variables currently are not imported at all (would be imported as static members)
  //print(OtherNamespace.namespaced_constexpr_int)
  //print(OtherNamespace.namespaced_static_constexpr_int)
  //print(MyClass.class_static_constexpr_int)

  print(MyClass().class_const_int)
  print(MyClass.class_static_const_int)

  // TODO: This seems to be incorrectly imported, this test here is just to check that the compiler doesn't crash.
  print(template_gcd)
}

// Only imported as external declarations:
// CHECK: sil_global public_external [let] @not_really_constant : $Int32

// CHECK:      sil shared [transparent] @$sSo9const_ints5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo9NamespaceO20namespaced_const_ints5Int32VvgZ : $@convention(method) (@thin Namespace.Type) -> Int32 {
// CHECK-NEXT: // %0 "self"
// CHECK-NEXT: bb0(%0 : $@thin Namespace.Type):
// CHECK-NEXT:   debug_value %0, let, name "self", argno 1
// CHECK-NEXT:   %2 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %3 = struct $Int32 (%2)
// CHECK-NEXT:   return %3
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo13constexpr_ints5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo20static_constexpr_ints5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo7MyClassV22class_static_const_ints5Int32VvgZ : $@convention(method) (@thin MyClass.Type) -> Int32 {
// CHECK-NEXT: // %0 "self"
// CHECK-NEXT: bb0(%0 : $@thin MyClass.Type):
// CHECK-NEXT:   debug_value %0, let, name "self", argno 1
// CHECK-NEXT:   %2 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %3 = struct $Int32 (%2)
// CHECK-NEXT:   return %3
// CHECK-NEXT: }
