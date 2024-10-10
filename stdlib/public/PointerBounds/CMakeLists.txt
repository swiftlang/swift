if (SWIFT_ENABLE_EXPERIMENTAL_POINTER_BOUNDS)
  add_swift_target_library(swift_PointerBounds ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    PointerBounds.swift
    SWIFT_COMPILE_FLAGS
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      -parse-stdlib

    LINK_FLAGS
      "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

    INSTALL_IN_COMPONENT stdlib
  )
endif()
