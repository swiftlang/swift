if(NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT)
  set(BUILTIN_FLOAT_SOURCES
    linker-support/magic-symbols-for-install-name.c
  )

  set(BUILTIN_FLOAT_GYB_SOURCES
    float.swift.gyb
  )

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(BUILTIN_FLOAT_SWIFT_FLAGS -Xfrontend -module-abi-name -Xfrontend Darwin)
  else()
    set(BUILTIN_FLOAT_SWIFT_FLAGS)
  endif()

  add_swift_target_library(swift_Builtin_float
      ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES}
      IS_SDK_OVERLAY
      
      ${BUILTIN_FLOAT_SOURCES}

      GYB_SOURCES
        ${BUILTIN_FLOAT_GYB_SOURCES}

      SWIFT_COMPILE_FLAGS
        ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
        ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
        ${BUILTIN_FLOAT_SWIFT_FLAGS}

      LINK_FLAGS
        ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

      INSTALL_IN_COMPONENT stdlib
      MACCATALYST_BUILD_FLAVOR zippered)

  # Embedded clang overlays - embedded libraries are built as .swiftmodule only,
  # i.e. there is no .o or .a file produced (no binary code is actually produced)
  # and only users of a library are going to actually compile any needed code.
  if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
    add_custom_target(embedded-builtin_float)
    add_dependencies(embedded-libraries embedded-builtin_float)

    add_embedded_swift_target_library(
      embedded-builtin_float
      swift_Builtin_float

      GYB_SOURCES
        ${BUILTIN_FLOAT_GYB_SOURCES}

      SWIFT_COMPILE_FLAGS
        ${BUILTIN_FLOAT_SWIFT_FLAGS}

      INSTALL_IN_COMPONENT stdlib
    )
  endif()
endif()
