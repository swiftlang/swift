// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  typealias c = a
}
struct d : e
  protocol e : a {
    associatedtype c where b == c.b
    associatedtype f = Self
