// RUN: %target-typecheck-verify-swift

#if _compiler_version("999.*.999.999.999")
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if _compiler_version("600.*.10.10")

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

#if _compiler_version("700a.*.10") // expected-error {{'_compiler_version' version check has invalid version '"700a.*.10"'}}
#endif

#if _compiler_version("...") // expected-error {{found empty version component}}
#endif

#if _compiler_version("") // expected-error {{found empty version component}}
  let y = 1
#else
  let thisWillStillParseBecauseConfigIsError = 1
#endif

#if _compiler_version("700.0.100") // expected-warning {{the second version component is not used for comparison in legacy compiler versions}}
#endif

#if _compiler_version("5.7.100") // expected-warning {{the second version component is not used for comparison in legacy compiler versions}}
#endif

#if _compiler_version("700.*.1.1.1.1") // expected-error {{version must not have more than five components}}
#endif

#if _compiler_version("9223372.*.1.1.1") // expected-error {{compiler version component '9223372' is not in the allowed range 0...9223371}}
#endif

#if _compiler_version("700.*.1000.1.1") // expected-error {{compiler version component '1000' is not in the allowed range 0...999}}
#endif

#if _compiler_version("700.*.1.1000.1") // expected-error {{compiler version component '1000' is not in the allowed range 0...999}}
#endif

#if _compiler_version("700.*.1.1.1000") // expected-error {{compiler version component '1000' is not in the allowed range 0...999}}
#endif

// New style _compiler_version()
#if _compiler_version(<4.0)
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if !_compiler_version(>=4.3.2.1.0)
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif
