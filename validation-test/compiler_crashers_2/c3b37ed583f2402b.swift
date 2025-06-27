// {"signature":"swift::TypeTransform<(anonymous namespace)::MapTypeIntoContext>::doIt(swift::Type, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b protocol
    c{associatedtype f : a<d> associatedtype d : a<f>} func e < b : c {
  b.f = b.d
