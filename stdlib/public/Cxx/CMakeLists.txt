list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../../cmake/modules)
include(StdlibOptions)

set(SWIFT_CXX_DEPS symlink_clang_headers)
if(SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT)
  list(APPEND SWIFT_CXX_DEPS copy-legacy-layouts)
endif()

add_swift_target_library(swiftCxx STATIC NO_LINK_NAME IS_STDLIB IS_SWIFT_ONLY
    CxxConvertibleToBool.swift
    CxxConvertibleToCollection.swift
    CxxDictionary.swift
    CxxPair.swift
    CxxOptional.swift
    CxxSet.swift
    CxxRandomAccessCollection.swift
    CxxSequence.swift
    CxxVector.swift
    CxxSpan.swift
    UnsafeCxxIterators.swift

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -cxx-interoperability-mode=default
    -enable-experimental-feature Span
    -enable-experimental-feature BuiltinModule
    -enable-experimental-feature AllowUnsafeAttribute
    -strict-memory-safety
    # This module should not pull in the C++ standard library, so we disable it explicitly.
    # For functionality that depends on the C++ stdlib, use C++ stdlib overlay (`swiftstd` module).
    -Xcc -nostdinc++

    DEPLOYMENT_VERSION_OSX ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_OSX}
    DEPLOYMENT_VERSION_IOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_IOS}
    DEPLOYMENT_VERSION_TVOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_TVOS}
    DEPLOYMENT_VERSION_WATCHOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_WATCHOS}
    DEPLOYMENT_VERSION_XROS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_XROS}

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    DEPENDS ${SWIFT_CXX_DEPS}
    MACCATALYST_BUILD_FLAVOR zippered
    INSTALL_IN_COMPONENT compiler
    INSTALL_BINARY_SWIFTMODULE NON_DARWIN_ONLY
    INSTALL_WITH_SHARED)

add_subdirectory(libstdcxx)
add_subdirectory(std)
add_subdirectory(cxxshim)
