
add_library(swiftCxxStdlib STATIC
  std.swift
  Chrono.swift
  String.swift)
set_target_properties(swiftCxxStdlib PROPERTIES
  Swift_MODULE_NAME CxxStdlib)
target_compile_options(swiftCxxStdlib PRIVATE
  "-strict-memory-safety"
  "-cxx-interoperability-mode=default"
  "SHELL:-enable-experimental-feature AllowUnsafeAttribute"
  # This flag is unnecessary when building with newer compilers that allow using
  # C++ symbols in resilient overlays (see f4204568).
  "SHELL:-enable-experimental-feature AssumeResilientCxxTypes"
  # The varying modularization of the C++ standard library on different
  # platforms makes it difficult to enable MemberImportVisibility for this
  # module
  "SHELL:-disable-upcoming-feature MemberImportVisibility"
  "SHELL:-Xfrontend -module-interface-preserve-types-as-written")
# NOTE: We need to setup the sysroot here as we need to ensure that we pick up
# the module.map from the C++ runtime for the `std` (spelt `CxxStdlib`) import.
target_compile_options(swiftCxxStdlib PRIVATE
  "$<$<PLATFORM_ID:Android>:SHELL:-Xcc --sysroot -Xcc ${CMAKE_ANDROID_NDK_TOOLCHAIN_UNIFIED}/sysroot>")
target_link_libraries(swiftCxxStdlib PRIVATE
  $<$<BOOL:${HAVE___GLIBCXX__}>:libstdcxx>
  $<$<NOT:$<PLATFORM_ID:Darwin>>:cxxshim>
  swiftCxx
  swiftCore
  swift_Builtin_float
  $<$<PLATFORM_ID:Android>:SwiftAndroid>
  $<$<PLATFORM_ID:Windows>:ClangModules>)

install(FILES std.apinotes
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/apinotes)
install(TARGETS swiftCxxStdlib
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxxStdlib)
install_swift_interface(swiftCxxStdlib)

embed_manifest(swiftCxxStdlib)
