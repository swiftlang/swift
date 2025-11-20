if(CMAKE_SYSTEM_NAME STREQUAL "Darwin" AND DEFINED SWIFT_STDLIB_LIBRARY_BUILD_TYPES)

  set(library_name "swiftCompatibilitySpan")

  add_swift_target_library("${library_name}" ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    FakeStdlib.swift
    ../../public/core/Span/MutableRawSpan.swift
    ../../public/core/Span/MutableSpan.swift
    ../../public/core/Span/OutputRawSpan.swift
    ../../public/core/Span/OutputSpan.swift
    ../../public/core/Span/RawSpan.swift
    ../../public/core/Span/Span.swift

    TARGET_SDKS ${SWIFT_APPLE_PLATFORMS}

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

    SWIFT_COMPILE_FLAGS
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      -parse-stdlib
      -module-abi-name Swift
      -DSPAN_COMPATIBILITY_STUB

    NO_SWIFTMODULE

    DEPLOYMENT_VERSION_OSX "10.14.4"
    DEPLOYMENT_VERSION_IOS "12.2"
    DEPLOYMENT_VERSION_TVOS "12.2"
    DEPLOYMENT_VERSION_WATCHOS "5.2"
    DEPLOYMENT_VERSION_XROS "1.0"
    DEPLOYMENT_VERSION_MACCATALYST "13.1"

    MACCATALYST_BUILD_FLAVOR "zippered"

    INSTALL_IN_COMPONENT compiler

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}" -Xlinker -not_for_dyld_shared_cache

    BACK_DEPLOYMENT_LIBRARY 6.2)
endif()
