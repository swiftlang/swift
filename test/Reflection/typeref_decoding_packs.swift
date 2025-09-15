// REQUIRES: no_asan
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// rdar://100805115
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// FIXME: rdar://127796117
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// RUN: %target-build-swift -target %target-swift-5.9-abi-triple %S/Inputs/Packs.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name TypesToReflect -o %t/%target-library-name(TypesToReflect)
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple %S/Inputs/Packs.swift -emit-module -emit-executable %no-fixup-chains -module-name TypesToReflect -o %t/TypesToReflect

// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) | %FileCheck %s
// RUN: %target-swift-reflection-dump %t/TypesToReflect | %FileCheck %s

// CHECK: FIELDS:
// CHECK: =======
// CHECK: TypesToReflect.Packed
// CHECK: ---------------------
// CHECK: tuple1: (repeat A)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (generic_type_parameter depth=0 index=0)
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: tuple2: (repeat (A) -> ())
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (function
// CHECK:         (parameters
// CHECK:           (generic_type_parameter depth=0 index=0)
// CHECK:         (result
// CHECK:           (tuple))
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: tuple3: (repeat TypesToReflect.Scalar<A>)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (bound_generic_struct TypesToReflect.Scalar
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: tuple4: (repeat Swift.Int)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (struct Swift.Int)
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: func1: (repeat A) -> ()
// CHECK: (function
// CHECK:   (parameters
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (generic_type_parameter depth=0 index=0)
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:   (result
// CHECK:     (tuple))

// CHECK: func2: (repeat TypesToReflect.Scalar<A>) -> ()
// CHECK: (function
// CHECK:   (parameters
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (bound_generic_struct TypesToReflect.Scalar
// CHECK:           (generic_type_parameter depth=0 index=0))
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:   (result
// CHECK:     (tuple))

// CHECK: func3: (repeat Swift.Int) -> ()
// CHECK: (function
// CHECK:   (parameters
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (struct Swift.Int)
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:   (result
// CHECK:     (tuple))

// CHECK: nominal1: TypesToReflect.AlsoPacked<Pack{repeat A}>
// CHECK: (bound_generic_struct TypesToReflect.AlsoPacked
// CHECK:   (pack
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (generic_type_parameter depth=0 index=0)
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))))

// CHECK: nominal2: TypesToReflect.AlsoPacked<Pack{repeat TypesToReflect.Scalar<A>}>
// CHECK: (bound_generic_struct TypesToReflect.AlsoPacked
// CHECK:   (pack
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (bound_generic_struct TypesToReflect.Scalar
// CHECK:           (generic_type_parameter depth=0 index=0))
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))))

// CHECK: nominal3: TypesToReflect.AlsoPacked<Pack{repeat Swift.Int}>
// CHECK: (bound_generic_struct TypesToReflect.AlsoPacked
// CHECK:   (pack
// CHECK:     (pack_expansion
// CHECK:       (pattern
// CHECK:         (struct Swift.Int)
// CHECK:       (count
// CHECK:         (generic_type_parameter depth=0 index=0))))

// CHECK: TypesToReflect.AlsoPacked
// CHECK: -------------------------
// CHECK: t: (repeat A)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (generic_type_parameter depth=0 index=0)
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: TypesToReflect.NestedPacked
// CHECK: ---------------------------
// CHECK: t: (repeat TypesToReflect.Packed<Pack{repeat A1}, A>)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (bound_generic_struct TypesToReflect.Packed
// CHECK:         (pack
// CHECK:           (pack_expansion
// CHECK:             (pattern
// CHECK:               (generic_type_parameter depth=1 index=0)
// CHECK:             (count
// CHECK:               (generic_type_parameter depth=1 index=0)))
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: u: (repeat TypesToReflect.AlsoPacked<Pack{repeat A1, A}>)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (bound_generic_struct TypesToReflect.AlsoPacked
// CHECK:         (pack
// CHECK:           (pack_expansion
// CHECK:             (pattern
// CHECK:               (generic_type_parameter depth=1 index=0)
// CHECK:             (count
// CHECK:               (generic_type_parameter depth=1 index=0))
// CHECK:           (generic_type_parameter depth=0 index=0)))
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))

// CHECK: ASSOCIATED TYPES:
// CHECK: =================
// CHECK: - TypesToReflect.AlsoPacked : TypesToReflect.P
// CHECK: typealias A = (repeat A)
// CHECK: (repeat A)
// CHECK: (tuple
// CHECK:   (pack_expansion
// CHECK:     (pattern
// CHECK:       (generic_type_parameter depth=0 index=0)
// CHECK:     (count
// CHECK:       (generic_type_parameter depth=0 index=0)))