// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-lifetime-resolution -verify \
// RUN:   -enable-experimental-feature LifetimeDependence %s

// REQUIRES: swift_feature_LifetimeDependence

func returnOptionalGeneric<T>(_ t: T) -> T? {
  guard .random() else { return nil }
  return t
}

enum Thinger<T> {
  case pair(T, T)
  case single(T)
}

func buildNestedEnum<T>(_ t: T) -> Thinger<T>? {
  if .random() { return .single(t) }
  if .random() { return .pair(t, t) }
  return nil
}
