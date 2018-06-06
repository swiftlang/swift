# Module import guard
if(DEFINED SWIFT_WINDOWS_SUPPORT_MODULE_LOADED)
  return()
endif()
set(SWIFT_WINDOWS_SUPPORT_MODULE_LOADED TRUE)


include(SwiftUtils)


function(swift_windows_arch_spelling arch result_var)
  if("${arch}" STREQUAL "i686")
    set(${result_var} "x86" PARENT_SCOPE)
  elseif("${arch}" STREQUAL "x86_64")
    set(${result_var} "x64" PARENT_SCOPE)
  elseif("${arch}" STREQUAL "armv7")
    set(${result_var} "arm" PARENT_SCOPE)
  elseif("${arch}" STREQUAL "aarch64")
    set(${result_var} "arm64" PARENT_SCOPE)
  else()
    message(FATAL_ERROR "do not know MSVC spelling for ARCH: `${arch}`")
  endif()
endfunction()


function(swift_verify_windows_environment_variables)
  precondition("$ENV{VCToolsInstallDir}"
    MESSAGE "VCToolsInstallDir environment variable must be set")
  precondition("$ENV{UniversalCRTSdkDir}"
    MESSAGE "UniversalCRTSdkDir environment variable must be set")
  precondition("$ENV{UCRTVersion}"
    MESSAGE "UCRTVersion environment variable must be set")
endfunction()


function(swift_windows_include_for_arch arch result_var)
  swift_verify_windows_environment_variables()

  set(paths
    "$ENV{VCToolsInstallDir}/include"
    "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/ucrt"
    "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/shared"
    "$ENV{UniversalCRTSdkDir}/Include/$ENV{UCRTVersion}/um")
  set(${result_var} "${paths}" PARENT_SCOPE)
endfunction()


function(swift_windows_lib_for_arch arch result_var)
  swift_verify_windows_environment_variables()
  swift_windows_arch_spelling("${arch}" windows_arch)

  set(paths)
  if("${windows_arch}" STREQUAL "x86")
    list(APPEND paths "$ENV{VCToolsInstallDir}/Lib")
  else()
    list(APPEND paths "$ENV{VCToolsInstallDir}/Lib/${windows_arch}")
  endif()
  list(APPEND paths
    "$ENV{UniversalCRTSdkDir}/Lib/$ENV{UCRTVersion}/ucrt/${windows_arch}"
    "$ENV{UniversalCRTSdkDir}/Lib/$ENV{UCRTVersion}/um/${windows_arch}")
  set(${result_var} "${paths}" PARENT_SCOPE)
endfunction()


function(swift_windows_generate_sdk_vfs_overlay flags)
  swift_verify_windows_environment_variables()

  get_filename_component(VCToolsInstallDir "$ENV{VCToolsInstallDir}" ABSOLUTE)
  get_filename_component(UniversalCRTSdkDir "$ENV{UniversalCRTSdkDir}" ABSOLUTE)
  set(UCRTVersion "$ENV{UCRTVersion}")

  # TODO(compnerd) use a target to avoid re-creating this file all the time
  configure_file(
    "${CMAKE_SOURCE_DIR}/utils/WindowsSDKVFSOverlay.yaml.in"
    "${CMAKE_BINARY_DIR}/windows-sdk-vfs-overlay.yaml"
    @ONLY)

  set(${flags}
    "-Xclang" "-ivfsoverlay"
    "-Xclang" "${CMAKE_BINARY_DIR}/windows-sdk-vfs-overlay.yaml"
    PARENT_SCOPE)
endfunction()

