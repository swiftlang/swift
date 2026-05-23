// {"kind":"typecheck","languageMode":6,"original":"54587afb","signature":"matchCallArgumentsImpl(llvm::SmallVectorImpl<swift::AnyFunctionType::Param>&, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::ParameterListInfo const&, std::__1::optional<unsigned int>, bool, swift::constraints::TrailingClosureMatching, swift::constraints::MatchCallArgumentListener&, llvm::SmallVectorImpl<llvm::SmallVector<unsigned int, 1u>>&)","signatureAssert":"Assertion failed: (args[fromArgIdx].getLabel().empty()), function matchCallArgumentsImpl","signatureNext":"matchCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
[UInt8](a
$_
: 0
