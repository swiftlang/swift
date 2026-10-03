
file(TO_CMAKE_PATH "$ENV{WindowsSdkDir}" WindowsSdkDir)
file(TO_CMAKE_PATH "$ENV{WindowsSDKVersion}" WindowsSDKVersion)
file(TO_CMAKE_PATH "$ENV{UniversalCRTSdkDir}" UniversalCRTSdkDir)
file(TO_CMAKE_PATH "$ENV{UCRTVersion}" UCRTVersion)
file(TO_CMAKE_PATH "$ENV{VCToolsInstallDir}" VCToolsInstallDir)
string(REGEX REPLACE "/+$" "" WindowsSDKVersion "${WindowsSDKVersion}")
string(REGEX REPLACE "/+$" "" UCRTVersion "${UCRTVersion}")

file(CONFIGURE
  OUTPUT windows-sdk-overlay.yaml
  CONTENT [[
---
version: 0
case-sensitive: false
use-external-names: true
roots:
  - name: "@WindowsSdkDir@/Include/@WindowsSDKVersion@"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/winsdk.modulemap"
      - name: WinSDK.apinotes
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/WinSDK.apinotes"
  - name: "@WindowsSdkDir@/Include/@WindowsSDKVersion@/um"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/winsdk_um.modulemap"
  - name: "@WindowsSdkDir@/Include/@WindowsSDKVersion@/shared"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/winsdk_shared.modulemap"
  - name: "@WindowsSdkDir@/Include/@WindowsSDKVersion@/winrt"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/winrt.modulemap"
      - name: SwiftWinRT.h
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/SwiftWinRT.h"
  - name: "@UniversalCRTSdkDir@/Include/@UCRTVersion@/ucrt"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/ucrt.modulemap"
      - name: SwiftUCRT.h
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/SwiftUCRT.h"
  - name: "@VCToolsInstallDir@/include"
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/vcruntime.modulemap"
      - name: vcruntime.apinotes
        type: file
        external-contents: "@CMAKE_CURRENT_SOURCE_DIR@/vcruntime.apinotes"
]]
ESCAPE_QUOTES @ONLY NEWLINE_STYLE LF)

add_library(ClangModules INTERFACE)
target_compile_options(ClangModules INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-vfsoverlay ${CMAKE_CURRENT_BINARY_DIR}/windows-sdk-overlay.yaml>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=\"${WindowsSdkDir}/Include/${WindowsSDKVersion}/module.modulemap\">")

install(TARGETS ClangModules
  EXPORT SwiftOverlayTargets)
install(FILES
  SwiftUCRT.h
  ucrt.modulemap
  SwiftWinRT.h
  WinSDK.apinotes
  vcruntime.apinotes
  vcruntime.modulemap
  winrt.modulemap
  winsdk.modulemap
  winsdk_um.modulemap
  winsdk_shared.modulemap
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})
