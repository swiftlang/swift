// {"signature":"void llvm::function_ref<void (swift::Type)>::callback_fn<(anonymous namespace)::typeEraseOpenedArchetypes(swift::Type)::$_0>(long, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
var a : String { get throws(b{
  a
