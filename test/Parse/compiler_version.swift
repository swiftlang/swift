// RUN: %target-parse-verify-swift

#if _compiler_version("999999.999999.999999.999999.999999")
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if !_compiler_version("777.7.7")
  // This shouldn't emit any diagnostics.
  $#%^*&
#endif

#if _compiler_version("700a.0.10") // expected-error {{invalid character in compiler version string}}
#endif

#if _compiler_version("...") // expected-error {{invalid character in compiler version string}}
#endif

#if _compiler_version("") // expected-error {{compiler version requirement is empty}}
  let y = 1
#else
  $#%^*&
#endif

#if _compiler_version("10.10.10.10") && os(iOS) // expected-error {{cannot combine _compiler_version with binary operators}}
#endif

#if _compiler_version("10.10.10.10") || os(iOS) // expected-error {{cannot combine _compiler_version with binary operators}}
#endif
