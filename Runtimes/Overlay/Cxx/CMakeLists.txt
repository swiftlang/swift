
include(CheckSymbolExists)
check_symbol_exists(_LIBCPP_VERSION "version" HAVE_LIBCPP_VERSION)
check_symbol_exists(__GLIBCXX__ "version" HAVE___GLIBCXX__)

if(NOT APPLE)
  add_subdirectory(cxxshim)
endif()
if(HAVE___GLIBCXX__)
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
  "$<$<COMPILE_LANGUAGE:Swift>:-cxx-interoperability-mode=default>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  # This module should not pull in the C++ standard library, so we disable it
  # explicitly.  For functionality that depends on the C++ stdlib, use C++
  # stdlib overlay (`swiftstd` module).
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -nostdinc++>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AllowUnsafeAttribute>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BuiltinModule>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Span>")
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
