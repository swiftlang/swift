
add_library(libstdcxx INTERFACE)
target_include_directories(libstdcxx INTERFACE
  $<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>>)

install(TARGETS libstdcxx
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
install(FILES
  libstdcxx.h
  libstdcxx.modulemap
  DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}")
