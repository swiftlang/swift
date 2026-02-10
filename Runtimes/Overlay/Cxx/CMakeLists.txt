
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
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>")
target_link_libraries(swiftCxx PUBLIC
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>
  swiftCore)

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftCxx INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(TARGETS swiftCxx
  EXPORT SwiftCxxOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCxx)
install_swift_interface(swiftCxx)

embed_manifest(swiftCxx)

export(EXPORT SwiftCxxOverlayTargets
  FILE "cmake/SwiftCxxOverlay/SwiftCxxOverlayTargets.cmake")
install(EXPORT SwiftCxxOverlayTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCxxOverlay"
  FILE "SwiftCxxOverlayTargets.cmake"
  COMPONENT SwiftOverlayCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftCxxOverlayConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCxxOverlay/SwiftCxxOverlayConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCxxOverlay")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCxxOverlay/SwiftCxxOverlayConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCxxOverlay/SwiftCxxOverlayConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCxxOverlay/SwiftCxxOverlayConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCxxOverlay")
