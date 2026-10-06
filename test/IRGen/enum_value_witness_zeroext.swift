// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// The getEnumTag value witness of a multi-payload enum returns the case index
// as an unsigned tag.  It must carry the zeroext return attribute so that
// targets which return sub-word integers in the low bits of a wider register
// without clearing the high bits (e.g. PowerPC64) extend the result.  Without
// it, swift_reflectionMirror_count() -> getFieldAt() consumes the tag as a
// full-width index (index * sizeof(FieldRecord)), reads stale high bits, and
// computes an out-of-bounds field-record address, crashing at -O when a
// multi-payload enum is reflected.

public class C {}

public enum MultiPayload {
  case none
  case a(C)
  case b(C)
  case c
}

// The emitted getEnumTag witness ("...Owug") must return zeroext i32.
// getEnumTagSinglePayload ("...Owet") receives the same attribute in
// getValueWitnessAttrs().
// CHECK: define {{.*}}zeroext i32 @"$s{{[0-9a-zA-Z_]*}}12MultiPayloadOwug"
