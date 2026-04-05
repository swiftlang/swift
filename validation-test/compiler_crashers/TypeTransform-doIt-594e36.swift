// {"kind":"typecheck","original":"2e27326c","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a {
  static  buildPartialBlock< b >(first : b) ->[b]static func buildPartialBlock(
    accumulated: , next b @a
  func c -> some Collection {
    c
