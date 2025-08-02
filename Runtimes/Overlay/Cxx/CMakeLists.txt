
if(NOT APPLE)
  add_subdirectory(cxxshim)
endif()
if(LINUX)
  add_ubdirectory(libstdcxx)
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
  "-warn-implicit-overrides"
  "SHELL:-Xfrontend -enable-lexical-lifetimes=false"
  "SHELL:-Xfrontend -enable-ossa-modules"
  # This module should not pull in the C++ standard library, so we disable it
  # explicitly.  For functionality that depends on the C++ stdlib, use C++
  # stdlib overlay (`swiftstd` module).
  "SHELL:-Xcc -nostdinc++"
  "SHELL:-enable-experimental-feature AllowUnsafeAttribute"
  "SHELL:-enable-experimental-feature BuiltinModule"
  "SHELL:-enable-experimental-feature InoutLifetimeDependence"
  "SHELL:-enable-experimental-feature LifetimeDependence"
  "SHELL:-enable-experimental-feature LifetimeDependenceMutableAccessors"
  "SHELL:-enable-experimental-feature MemberImportVisibility"
  "SHELL:-enable-experimental-feature NoncopyableGenerics2"
  "SHELL:-enable-experimental-feature NonescapableTypes"
  "SHELL:-enable-experimental-feature Span"
  "SHELL:-enable-experimental-feature SE427NoInferenceOnExtension"
  "SHELL:-enable-experimental-feature SuppressedAssociatedTypes")
target_link_libraries(swiftCxx PRIVATE
  swiftCore)

install(TARGETS swiftCxx
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxx)
install_swift_interface(swiftCxx)

embed_manifest(swiftCxx)
