function(swift_android_include_for_arch arch var)
  set(paths)
  list(APPEND paths
       "${SWIFT_ANDROID_NDK_PATH}/sources/cxx-stl/llvm-libc++/include"
       "${SWIFT_ANDROID_NDK_PATH}/sources/cxx-stl/llvm-libc++abi/include"
       "${SWIFT_ANDROID_NDK_PATH}/sources/android/support/include"
       "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include"
       "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include/${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}") 
  set(${var} ${paths} PARENT_SCOPE)
endfunction()

function(swift_android_lib_for_arch arch var)
  set(paths)

  if(arch STREQUAL armv7) 
    list(APPEND paths
         "${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_PREBUILT_PATH}/${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}/lib/armv7-a")
  endif()

  list(APPEND paths
       "${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_PREBUILT_PATH}/lib/gcc/${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}/${SWIFT_ANDROID_NDK_GCC_VERSION}.x")
  set(${var} ${paths} PARENT_SCOPE)
endfunction()
