set(SWIFT_CXX_LIBRARY_KIND STATIC)
if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "WINDOWS")
  set(SWIFT_CXX_LIBRARY_KIND SHARED)
endif()

add_swift_target_library(swiftCxx ${SWIFT_CXX_LIBRARY_KIND} NO_LINK_NAME IS_STDLIB
    CxxConvertibleToCollection.swift
    CxxRandomAccessCollection.swift
    CxxSequence.swift

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -Xfrontend -enable-experimental-cxx-interop
    # This module should not pull in the C++ standard library, so we disable it explicitly.
    # For functionality that depends on the C++ stdlib, use C++ stdlib overlay (`swiftstd` module).
    -Xcc -nostdinc++

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    INSTALL_IN_COMPONENT compiler
    INSTALL_WITH_SHARED)

add_subdirectory(std)
add_subdirectory(cxxshim)
