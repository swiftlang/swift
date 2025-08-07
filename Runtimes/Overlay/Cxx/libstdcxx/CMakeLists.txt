
add_library(libstdcxx INTERFACE)
target_compile_options(libstdcxx INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${CMAKE_CURRENT_SOURCE_DIR}/libstdcxx.modulemap>")
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
