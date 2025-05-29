// {"signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Combine extension Publishers.Share{a <b , c where Upstream == Publishers.FlatMap <b, c>{func a <c where Upstream == Publishers.FlatMap <b, c>, c.Output ==
