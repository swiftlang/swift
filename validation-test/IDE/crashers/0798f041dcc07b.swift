// {"kind":"complete","signature":"matchCallArgumentsImpl(llvm::SmallVectorImpl<swift::AnyFunctionType::Param>&, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::ParameterListInfo const&, std::__1::optional<unsigned int>, bool, swift::constraints::TrailingClosureMatching, swift::constraints::MatchCallArgumentListener&, llvm::SmallVectorImpl<llvm::SmallVector<unsigned int, 1u>>&)::$_5::operator()(unsigned int&, swift::Identifier, bool, bool) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
struct a {
  @dynamicMemberLookup enum b {
    subscript <c>(dynamicMember d: KeyPath<a, c>) -> c {
      @dynamicMemberLookup class e {c { self[
#^COMPLETE^#}
        subscript(dynamicMember d: KeyPath<b, c>) -> c
      }
    }
  }
}
