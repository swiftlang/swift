
add_library(cxxshim INTERFACE)
target_include_directories(cxxshim INTERFACE
  $<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>>)

install(TARGETS cxxshim
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
install(FILES
  libcxxshim.h
  libcxxshim.modulemap
  libcxxstdlibshim.h
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/${SwiftOverlay_PLATFORM_SUBDIR}")
