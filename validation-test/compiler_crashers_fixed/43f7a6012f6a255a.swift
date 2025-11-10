// {"kind":"emit-ir","original":"493cc9db","signature":"(anonymous namespace)::TypeContextDescriptorBuilderBase<(anonymous namespace)::StructContextDescriptorBuilder, swift::StructDecl>::emit()"}
// RUN: not %target-swift-frontend -emit-ir %s
class a<b> {
}
@available(SwiftStdlib 6.2, *)
struct c<let d: a<e>, e> {
}
