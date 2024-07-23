if(NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT)
  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(BUILTIN_FLOAT_SWIFT_FLAGS -Xfrontend -module-abi-name -Xfrontend Darwin)
  else()
    set(BUILTIN_FLOAT_SWIFT_FLAGS)
  endif()

  add_swift_target_library(swift_Builtin_float
      ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES}
      IS_SDK_OVERLAY
      
      linker-support/magic-symbols-for-install-name.c

      GYB_SOURCES
        float.swift.gyb

      SWIFT_COMPILE_FLAGS
        ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
        ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
        ${BUILTIN_FLOAT_SWIFT_FLAGS}

      LINK_FLAGS
        ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

      INSTALL_IN_COMPONENT stdlib
      MACCATALYST_BUILD_FLAVOR zippered)
endif()
