// RUN: %target-parse-verify-swift

#if _compiler_version("999999.*.999999.999999.999999")
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if _compiler_version("10.*.10.10")

#if os(iOS)
  let z = 1
#else
  let z = 1
#endif

#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#if os(iOS)
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if !_compiler_version("777.*.7")
  // This shouldn't emit any diagnostics.
  $#%^*&
#endif

#if _compiler_version("700a.*.10") // expected-error {{compiler version component is not a number}}
#endif

#if _compiler_version("...") // expected-error {{found empty compiler version component}}
// expected-error@-1 {{found empty compiler version component}}
// expected-error@-2 {{found empty compiler version component}}
#endif

#if _compiler_version("") // expected-error {{compiler version requirement is empty}}
  let y = 1
#else
  let thisWillStillParseBecauseConfigIsError = 1
#endif

#if _compiler_version("10.*.10.10") && os(iOS) // expected-error {{cannot combine _compiler_version with binary operators}}
#endif

#if _compiler_version("10.*.10.10") || os(iOS) // expected-error {{cannot combine _compiler_version with binary operators}}
#endif

#if os(iOS) && _compiler_version("10.*.10.10")  // expected-error {{cannot combine _compiler_version with binary operators}}
#endif

#if os(iOS) || _compiler_version("10.*.10.10")  // expected-error {{cannot combine _compiler_version with binary operators}}
#endif

#if _compiler_version("700.0.100") // expected-warning {{the second version component is not used for comparison}}
#endif
