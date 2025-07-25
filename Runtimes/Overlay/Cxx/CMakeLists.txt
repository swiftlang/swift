
add_subdirectory(cxxshim)
if(LINUX)
  add_subdirectory(libstdcxx)
endif()
add_subdirectory(std)

add_library(swiftCxx STATIC
  CxxConvertibleToBool.swift
  CxxConvertibleToCollection.swift
  CxxDictionary.swift
  CxxOptional.swift
  CxxPair.swift
  CxxRandomAccessCollection.swift
  CxxSequence.swift
  CxxSet.swift
  CxxSpan.swift
  CxxVector.swift
  UnsafeCxxIterators.swift)
set_target_properties(swiftCxx PROPERTIES
  Swift_MODULE_NAME Cxx)
target_compile_options(swiftCxx PRIVATE
  "-strict-memory-safety"
  "-cxx-interoperability-mode=default"
  # This module should not pull in the C++ standard library, so we disable it
  # explicitly.  For functionality that depends on the C++ stdlib, use C++
  # stdlib overlay (`swiftstd` module).
  "SHELL:-Xcc -nostdinc++"
  "SHELL:-enable-experimental-feature Span"
  "SHELL:-enable-experimental-feature BuiltinModule"
  "SHELL:-enable-experimental-feature AllowUnsafeAttribute")

install(TARGETS swiftCxx
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxx)
install_swift_interface(swiftCxx)

embed_manifest(swiftCxx)
