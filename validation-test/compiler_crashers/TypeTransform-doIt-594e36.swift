// {"kind":"typecheck","original":"2e27326c","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","stackOverflow":true}
// This test crashes by overflowing the stack, set a suitable timeout to ensure it doesn't take too long.
// RUN: not %{python} %swift_src_root/test/Inputs/timeout.py 60 \
// RUN:             %target-swift-frontend -typecheck %s || \
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a {
  static  buildPartialBlock< b >(first : b) ->[b]static func buildPartialBlock(
    accumulated: , next b @a
  func c -> some Collection {
    c
