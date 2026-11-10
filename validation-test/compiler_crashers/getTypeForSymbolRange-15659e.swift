// {"kind":"typecheck","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureNext":"PropertyMap::getTypeForTerm"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Combine extension Publishers.Share{a <b , c where Upstream == Publishers.FlatMap <b, c>{func a <c where Upstream == Publishers.FlatMap <b, c>, c.Output ==
