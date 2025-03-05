add_compile_options(
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature TypedThrows>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Macros>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature FreestandingMacros>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BitwiseCopyable>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature ValueGenerics>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AddressableParameters>")
