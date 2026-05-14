
file(CONFIGURE
  OUTPUT wasi-overlay.yaml
  CONTENT [[
---
version: 0
case-sensitive: false
use-external-names: true
roots:
  - name: "@CMAKE_SYSROOT@/include/@CMAKE_C_COMPILER_TARGET@"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/wasi-libc.modulemap"
      - name: SwiftWASILibc.apinotes
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/SwiftWASILibc.apinotes"
]]
ESCAPE_QUOTES @ONLY NEWLINE_STYLE LF)

add_library(SwiftWASILibc INTERFACE)
target_compile_options(SwiftWASILibc INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc --sysroot=\"${CMAKE_SYSROOT}\">"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-vfsoverlay ${CMAKE_CURRENT_BINARY_DIR}/wasi-overlay.yaml>")

install(TARGETS SwiftWASILibc
  EXPORT SwiftOverlayTargets)
install(FILES
  wasi-libc.modulemap
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/${SwiftOverlay_PLATFORM_SUBDIR}/${SwiftOverlay_ARCH_SUBDIR})
install(FILES
  SwiftWASILibc.apinotes
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/apinotes)
