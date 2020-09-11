// RUN: %target-swift-frontend -emit-sil -emit-sorted-sil %s | %FileCheck %s

// MARK: - Public functions

@differentiable
@_silgen_name("f000_invokedDirectlyByDifferentiableAttrPublic")
public func f000_invokedDirectlyByDifferentiableAttrPublic(_ x: Float) -> Float {
  return f001_invokedIndirectlyByDifferentiableAttrPublic(x)
}
// CHECK-LABEL: sil_differentiability_witness [serialized] [parameters 0] [results 0] @f000_invokedDirectlyByDifferentiableAttrPublic
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f001_invokedIndirectlyByDifferentiableAttrPublic")
public func f001_invokedIndirectlyByDifferentiableAttrPublic(_ x: Float) -> Float {
  return x
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f001_invokedIndirectlyByDifferentiableAttrPublic
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f002_invokedDirectlyByConversionPublic")
public func f002_invokedDirectlyByConversionPublic(_ x: Float) -> Float {
  return f003_invokedIndirectlyByConversionPublic(x)
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f002_invokedDirectlyByConversionPublic
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f003_invokedIndirectlyByConversionPublic")
public func f003_invokedIndirectlyByConversionPublic(_ x: Float) -> Float {
  return x
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f003_invokedIndirectlyByConversionPublic
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

// MARK: - Internal functions

@differentiable
@_silgen_name("f004_invokedDirectlyByDifferentiableAttrInternal")
internal func f004_invokedDirectlyByDifferentiableAttrInternal(_ x: Float) -> Float {
  return f005_invokedIndirectlyByDifferentiableAttrInternal(x)
}
// CHECK-LABEL: sil_differentiability_witness hidden [parameters 0] [results 0] @f004_invokedDirectlyByDifferentiableAttrInternal
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f005_invokedIndirectlyByDifferentiableAttrInternal")
internal func f005_invokedIndirectlyByDifferentiableAttrInternal(_ x: Float) -> Float {
  return x
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f005_invokedIndirectlyByDifferentiableAttrInternal
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f006_invokedDirectlyByConversionInternal")
internal func f006_invokedDirectlyByConversionInternal(_ x: Float) -> Float {
  return f007_invokedIndirectlyByConversionInternal(x)
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f006_invokedDirectlyByConversionInternal
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

@_silgen_name("f007_invokedIndirectlyByConversionInternal")
internal func f007_invokedIndirectlyByConversionInternal(_ x: Float) -> Float {
  return x
}
// CHECK-LABEL: sil_differentiability_witness private [parameters 0] [results 0] @f007_invokedIndirectlyByConversionInternal
// CHECK-NEXT: jvp
// CHECK-NEXT: vjp

func invokesByConversion() -> Float {
  var result: Float = 0
  result += gradient(at: 0, in: f002_invokedDirectlyByConversionPublic)
  result += gradient(at: 0, in: f006_invokedDirectlyByConversionInternal)
  return result
}
