// {"kind":"typecheck","original":"006e7b19","signature":"(anonymous namespace)::fixItOverrideDeclarationTypesImpl(swift::ValueDecl*, swift::ValueDecl const*, llvm::SmallVectorImpl<std::__1::tuple<(anonymous namespace)::NoteKind_t, swift::SourceRange, std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char>>>>&)","signatureNext":"computeFixitsForOverriddenDeclaration"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  macro b()
  struct c: a {
    macro b()
  }
}
