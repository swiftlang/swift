// RUN: %target-typecheck-verify-swift

#if _compiler_version("999.*.999.999.999")
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
  %#^*&
#endif

#if _compiler_version("700a.*.10") // expected-error {{version component contains non-numeric characters}}
#endif

#if _compiler_version("...") // expected-error {{found empty version component}}
// expected-error@-1 {{found empty version component}}
// expected-error@-2 {{found empty version component}}
#endif

#if _compiler_version("") // expected-error {{version requirement is empty}}
  let y = 1
#else
  let thisWillStillParseBecauseConfigIsError = 1
#endif

#if _compiler_version("700.0.100") // expected-warning {{the second version component is not used for comparison}}
#endif

#if _compiler_version("700.*.1.1.1.1") // expected-error {{version must not have more than five components}}
#endif

#if _compiler_version("9223372.*.1.1.1") // expected-error {{version component out of range: must be in [0, 9223371]}}
#endif

#if _compiler_version("700.*.1000.1.1") // expected-error {{version component out of range: must be in [0, 999]}}
#endif

#if _compiler_version("700.*.1.1000.1") // expected-error {{version component out of range: must be in [0, 999]}}
#endif

#if _compiler_version("700.*.1.1.1000") // expected-error {{version component out of range: must be in [0, 999]}}
#endif
