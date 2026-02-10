
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
  "SHELL:-enable-experimental-feature Lifetimes"
  # This flag is unnecessary when building with newer compilers that allow using
  # C++ symbols in resilient overlays (see f4204568).
  "SHELL:-enable-experimental-feature AssumeResilientCxxTypes"
  # The varying modularization of the C++ standard library on different
  # platforms makes it difficult to enable MemberImportVisibility for this
  # module or qualify types with module names
  "SHELL:-disable-upcoming-feature MemberImportVisibility"
  "SHELL:-Xfrontend -module-interface-preserve-types-as-written"
  "SHELL:-Xfrontend -disable-module-selectors-in-module-interface")

target_link_libraries(swiftCxxStdlib PUBLIC
  $<$<BOOL:${HAVE___GLIBCXX__}>:libstdcxx>
  $<$<NOT:$<PLATFORM_ID:Darwin>>:cxxshim>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>
  swiftCxx
  swiftCore
  swift_Builtin_float
  $<$<PLATFORM_ID:Android>:SwiftAndroid>
  $<$<PLATFORM_ID:Windows>:ClangModules>)

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftCxxStdlib INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(FILES std.apinotes
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/apinotes)
install(TARGETS swiftCxxStdlib
  EXPORT SwiftCxxOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxxStdlib)
install_swift_interface(swiftCxxStdlib)

embed_manifest(swiftCxxStdlib)
