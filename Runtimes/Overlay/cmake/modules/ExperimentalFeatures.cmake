add_compile_options(
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypesWithDefaults>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BorrowingSequence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>")
