
# FIXME: how do we determine the sysroot? `CMAKE_SYSROOT` does not contain the sysroot.
file(CONFIGURE
  OUTPUT android-ndk-overlay.yaml
  CONTENT [[
---
version: 0
case-sensitive: false
use-external-names: false
roots:
  - name: "@CMAKE_ANDROID_NDK@/toolchains/llvm/prebuilt/windows-x86_64/sysroot/usr/include"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/android.modulemap"
      - name: SwiftAndroidNDK.h
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/SwiftAndroidNDK.h"
      - name: SwiftBionic.h
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/SwiftBionic.h"
]]
ESCAPE_QUOTES @ONLY NEWLINE_STYLE LF)

add_library(SwiftAndroid INTERFACE)
target_compile_options(SwiftAndroid INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc --sysroot=\"${CMAKE_ANDROID_NDK_TOOLCHAIN_UNIFIED}/sysroot\">"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-vfsoverlay ${CMAKE_CURRENT_BINARY_DIR}/android-ndk-overlay.yaml>")

install(FILES
  android.modulemap
  SwiftAndroidNDK.h
  SwiftBionic.h
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/${SwiftOverlay_PLATFORM_SUBDIR}/${SwiftOverlay_ARCH_SUBDIR})

install(FILES
  posix_filesystem.apinotes
  spawn.apinotes
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/swift/apinotes)
