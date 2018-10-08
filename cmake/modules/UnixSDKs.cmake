
function(swift_icu_variables_set sdk arch result)
  string(TOUPPER "${sdk}" sdk)

  set(icu_var_ICU_UC_INCLUDE ${SWIFT_${sdk}_${arch}_ICU_UC_INCLUDE})
  set(icu_var_ICU_UC ${SWIFT_${sdk}_${arch}_ICU_UC})
  set(icu_var_ICU_I18N_INCLUDE ${SWIFT_${sdk}_${arch}_ICU_I18N_INCLUDE})
  set(icu_var_ICU_I18N ${SWIFT_${sdk}_${arch}_ICU_I18N})

  if(icu_var_ICU_UC_INCLUDE AND icu_var_ICU_UC AND
     icu_var_ICU_I18N_INCLUDE AND icu_var_ICU_I18N)
    set(${result} TRUE PARENT_SCOPE)
  else()
    set(${result} FALSE PARENT_SCOPE)
  endif()
endfunction()

function(swift_verify_icu_configured sdk)
  if(NOT SWIFT_BUILD_STDLIB AND NOT SWIFT_BUILD_SDK_OVERLAY)
    return()
  endif()

  foreach(arch IN LISTS SWIFT_SDK_${sdk}_ARCHITECTURES)
    swift_icu_variables_set(${sdk} ${arch} SWIFT_${sdk}_${arch}_ICU_CONFIGURED)
    if(SWIFT_${sdk}_${arch}_ICU_CONFIGURED)
    elseif(${SWIFT_HOST_VARIANT_SDK} STREQUAL ${sdk} AND
           ${SWIFT_HOST_VARIANT_ARCH} STREQUAL ${arch})
      find_package(ICU REQUIRED COMPONENTS uc i18n)
    elseif(SWIFT_PATH_TO_LIBICU_BUILD)
      # TODO(compnerd) we need to generalize this to each os/arch combination
    else()
      message(SEND_ERROR "ICU for ${sdk} ${arch} not configured")
    endif()
  endforeach()
endfunction()

# Should we cross-compile the standard library for Android?
is_sdk_requested(ANDROID swift_build_android)
if(swift_build_android)
  precondition(NOT SWIFT_ANDROID_NDK_PATH STREQUAL ""
               MESSAGE "android NDK path must be specified")
  precondition(CMAKE_HOST_SYSTEM_NAME STREQUAL Darwin OR CMAKE_HOST_SYSTEM_NAME STREQUAL Linux
               MESSAGE "android requires a Darwin or Linux host to build the runtime")

  if("${SWIFT_SDK_ANDROID_ARCHITECTURES}" STREQUAL "")
    set(SWIFT_SDK_ANDROID_ARCHITECTURES armv7;aarch64)
  endif()
  configure_sdk_unix("Android" "${SWIFT_SDK_ANDROID_ARCHITECTURES}")
  swift_verify_icu_configured(ANDROID)
endif()

is_sdk_requested(CYGWIN swift_build_cygwin)
if(swift_build_cygwin)
  if("${SWIFT_SDK_CYGWIN_ARCHITECTURES}" STREQUAL "")
    set(SWIFT_SDK_CYGWIN_ARCHITECTURES x86_64)
  endif()
  configure_sdk_unix("Cygwin" "${SWIFT_SDK_CYGWIN_ARCHITECTURES}")
  swift_verify_icu_configured(CYGWIN)
endif()

is_sdk_requested(FREEBSD swift_build_freebsd)
if(swift_build_freebsd)
  if("${SWIFT_SDK_FREEBSD_ARCHITECTURES}" STREQUAL "")
    set(SWIFT_SDK_FREEBSD_ARCHITECTURES x86_64)
  endif()
  configure_sdk_unix("FreeBSD" "${SWIFT_SDK_FREEBSD_ARCHITECTURES}")
  swift_verify_icu_configured(FREEBSD)
endif()

is_sdk_requested(HAIKU swift_build_haiku)
if(swift_build_haiku)
  if("${SWIFT_SDK_HAIKU_ARCHITECTURES}" STREQUAL "")
    set(SWIFT_SDK_HAIKU_ARCHITECTURES x86_64)
  endif()
  configure_sdk_unix("Haiku" "${SWIFT_SDK_HAIKU_ARCHITECTURES}")
  swift_verify_icu_configured(HAIKU)
endif()

is_sdk_requested(LINUX swift_build_linux)
if(swift_build_linux)
  if("${SWIFT_SDK_LINUX_ARCHITECTURES}" STREQUAL "")
    # set(SWIFT_SDK_LINUX_ARCHITECTURES armv6;armv7;i686;powerpc64;powerpc64le;s390x;x86_64)
    set(SWIFT_SDK_LINUX_ARCHITECTURES ${SWIFT_HOST_VARIANT_ARCH})
  endif()
  configure_sdk_unix("Linux" "${SWIFT_SDK_LINUX_ARCHITECTURES}")
  swift_verify_icu_configured(LINUX)
endif()

