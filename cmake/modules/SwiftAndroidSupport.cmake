function(swift_android_libcxx_include_paths var)
  set(${var}
        "${SWIFT_ANDROID_NDK_PATH}/sources/cxx-stl/llvm-libc++/include"
        "${SWIFT_ANDROID_NDK_PATH}/sources/cxx-stl/llvm-libc++abi/include"
      PARENT_SCOPE)
endfunction()

function(swift_android_include_for_arch arch var)
  set(paths)
  list(APPEND paths
       "${SWIFT_ANDROID_NDK_PATH}/sources/android/support/include"
       "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include"
       "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include/${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}") 
  set(${var} ${paths} PARENT_SCOPE)
endfunction()

function(swift_android_lib_for_arch arch var)
  set(_prebuilt "${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_PREBUILT_PATH}")
  set(_host "${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}")

  set(paths)
  if(arch STREQUAL armv7)
    list(APPEND paths "${_prebuilt}/${_host}/lib/armv7-a")
  elseif(arch STREQUAL aarch64)
    list(APPEND paths "${_prebuilt}/${_host}/lib64")
  else()
    message(SEND_ERROR "unknown architecture (${arch}) for android")
  endif()
  list(APPEND paths "${_prebuilt}/lib/gcc/${_host}/${SWIFT_ANDROID_NDK_GCC_VERSION}.x")

  set(${var} ${paths} PARENT_SCOPE)
endfunction()
