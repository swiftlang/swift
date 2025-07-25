
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
target_link_libraries(swiftCxxStdlib PRIVATE
  cxxshim
  swift_Builtin_float
  $<$<PLATFORM_ID:Android>:swiftAndroid>)
target_link_libraries(swiftCxxStdlib PRIVATE
  $<$<PLATFORM_ID:Linux>:libstdcxx)

install(TARGETS swiftCxxStdlib
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxxStdlib)
install_swift_interface(swiftCxxStdlib)

embed_manifest(swiftCxxStdlib)
