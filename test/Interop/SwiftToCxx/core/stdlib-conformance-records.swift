// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/stdlib-conformances.swift -module-name StdlibConf -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/stdlib-conf.h -cxx-interoperability-mode=upcoming-swift

// RUN: %FileCheck %s < %t/stdlib-conf.h

// REQUIRES: objc_interop

// Verify that conformance records for stdlib protocols (Equatable,
// Hashable, Comparable) are emitted in the generated C++ header for
// same-module conformances.

// --- Conformance descriptor extern declarations ---

// CHECK-LABEL: // Conformance records for stdlib protocols.
// CHECK-NEXT: namespace _impl {
// CHECK-DAG: SWIFT_EXTERN const char $s10StdlibConf5PointVSQAAMc[];
// CHECK-DAG: SWIFT_EXTERN const char $s10StdlibConf5PointVSHAAMc[];
// CHECK-DAG: SWIFT_EXTERN const char $s10StdlibConf11TemperatureVSLAAMc[];
// CHECK-DAG: SWIFT_EXTERN const char $s10StdlibConf11TemperatureVSQAAMc[];
// CHECK: } // namespace _impl

// --- ADL using declarations for free operators ---

// CHECK: #ifdef __cpp_concepts
// CHECK-DAG: using ::operator==;
// CHECK-DAG: using ::operator!=;
// CHECK-DAG: using ::operator<;
// CHECK-DAG: using ::operator<=;
// CHECK-DAG: using ::operator>;
// CHECK-DAG: using ::operator>=;
// CHECK: #endif

// --- Conformance record specializations ---

// CHECK: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"

// CHECK-DAG: struct swift::EquatableConformance<StdlibConf::Point>
// CHECK-DAG: static inline const void* getWitnessTable()
// CHECK-DAG: swift_getWitnessTable
// CHECK-DAG: struct swift::HashableConformance<StdlibConf::Point>
// CHECK-DAG: struct swift::EquatableConformance<StdlibConf::Temperature>
// CHECK-DAG: struct swift::ComparableConformance<StdlibConf::Temperature>

// CHECK: #pragma clang diagnostic pop
