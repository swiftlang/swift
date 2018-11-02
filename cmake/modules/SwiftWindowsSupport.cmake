
include(SwiftUtils)

function(swift_windows_arch_spelling arch var)
  if(${arch} STREQUAL i686)
    set(${var} x86 PARENT_SCOPE)
  elseif(${arch} STREQUAL x86_64)
    set(${var} x64 PARENT_SCOPE)
  elseif(${arch} STREQUAL armv7)
    set(${var} arm PARENT_SCOPE)
  elseif(${arch} STREQUAL aarch64)
    set(${var} arm64 PARENT_SCOPE)
  else()
    message(FATAL_ERROR "do not know MSVC spelling for ARCH: `${arch}`")
  endif()
endfunction()

function(swift_verify_windows_environment_variables)
  set(VCToolsInstallDir $ENV{VCToolsInstallDir})
  set(UniversalCRTSdkDir $ENV{UniversalCRTSdkDir})
  set(UCRTVersion $ENV{UCRTVersion})

  precondition(VCToolsInstallDir
               MESSAGE
                 "VCToolsInstallDir environment variable must be set")
  precondition(UniversalCRTSdkDir
               MESSAGE
                 "UniversalCRTSdkDir environment variable must be set")
  precondition(UCRTVersion
               MESSAGE
                 "UCRTVersion environment variable must be set")
endfunction()

function(swift_windows_include_for_arch arch var)
  swift_verify_windows_environment_variables()

  set(paths
        "$ENV{VCToolsInstallDir}/include"
        "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/ucrt"
        "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/shared"
        "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/um")
  set(${var} ${paths} PARENT_SCOPE)
endfunction()

function(swift_windows_lib_for_arch arch var)
  swift_verify_windows_environment_variables()
  swift_windows_arch_spelling(${arch} ARCH)

  set(paths)
  if(${ARCH} STREQUAL x86)
    list(APPEND paths "$ENV{VCToolsInstallDir}/Lib")
  else()
    list(APPEND paths "$ENV{VCToolsInstallDir}/Lib/${ARCH}")
  endif()
  list(APPEND paths
          "$ENV{UniversalCRTSdkDir}/Lib/$ENV{UCRTVersion}/ucrt/${ARCH}"
          "$ENV{UniversalCRTSdkDir}/Lib/$ENV{UCRTVersion}/um/${ARCH}")
  set(${var} ${paths} PARENT_SCOPE)
endfunction()

function(swift_windows_generate_sdk_vfs_overlay flags)
  swift_verify_windows_environment_variables()

  get_filename_component(VCToolsInstallDir $ENV{VCToolsInstallDir} ABSOLUTE)
  get_filename_component(UniversalCRTSdkDir $ENV{UniversalCRTSdkDir} ABSOLUTE)
  set(UCRTVersion $ENV{UCRTVersion})

  # TODO(compnerd) use a target to avoid re-creating this file all the time
  configure_file("${CMAKE_SOURCE_DIR}/utils/WindowsSDKVFSOverlay.yaml.in"
                 "${CMAKE_BINARY_DIR}/windows-sdk-vfs-overlay.yaml"
                 @ONLY)

  set(${flags}
      -Xclang;-ivfsoverlay;-Xclang;"${CMAKE_BINARY_DIR}/windows-sdk-vfs-overlay.yaml"
      PARENT_SCOPE)
endfunction()

