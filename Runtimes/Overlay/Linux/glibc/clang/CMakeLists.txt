gyb_expand(glibc.modulemap.gyb glibc.modulemap
  FLAGS -DCMAKE_SDK="LINUX")
gyb_expand(SwiftGlibc.h.gyb SwiftGlibc.h)

file(CONFIGURE
  OUTPUT glibc-overlay.yml
  CONTENT [==[
---
version: 0
case-sensitive: false
use-external-names: true
roots:
  - name: "/usr/include"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_BINARY_DIR@/glibc.modulemap"
      - name: SwiftGlibc.h
        type: file
        external-contents: "@CMAKE_CURRENT_BINARY_DIR@/SwiftGlibc.h"
]==]
  ESCAPE_QUOTES @ONLY NEWLINE_STYLE LF)

add_library(SwiftGlibcClangOverlay INTERFACE
  SwiftGlibc.h
  glibc.modulemap)
target_compile_options(SwiftGlibcClangOverlay INTERFACE
  "$<BUILD_INTERFACE:$<$<COMPILE_LANGUAGE:Swift>:SHELL:-vfsoverlay ${CMAKE_CURRENT_BINARY_DIR}/glibc-overlay.yml>>")

install(TARGETS SwiftGlibcClangOverlay EXPORT SwiftOverlayTargets)

install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/glibc.modulemap"
    "${CMAKE_CURRENT_BINARY_DIR}/SwiftGlibc.h"
  DESTINATION
    "${CMAKE_INSTALL_LIBDIR}/swift/${SwiftOverlay_PLATFORM_SUBDIR}/${SwiftOverlay_ARCH_SUBDIR}")
