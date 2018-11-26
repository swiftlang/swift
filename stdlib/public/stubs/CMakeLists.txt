set(swift_stubs_sources
    Assert.cpp
    CommandLine.cpp
    GlobalObjects.cpp
    KeyPaths.cpp
    LibcShims.cpp
    Random.cpp
    Stubs.cpp
    ThreadLocalStorage.cpp
    MathStubs.cpp
)
set(swift_stubs_objc_sources
    Availability.mm
    FoundationHelpers.mm
    OptionalBridgingHelper.mm
    Reflection.mm
    SwiftNativeNSXXXBaseARC.m
    SwiftNativeNSXXXBase.mm.gyb)
set(swift_stubs_unicode_normalization_sources
    UnicodeNormalization.cpp)
set(LLVM_OPTIONAL_SOURCES
    ${swift_stubs_objc_sources}
    ${swift_stubs_unicode_normalization_sources})

set(swift_stubs_c_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
list(APPEND swift_stubs_c_compile_flags -DswiftCore_EXPORTS)
list(APPEND swift_stubs_c_compile_flags -I${SWIFT_SOURCE_DIR}/include)

add_swift_target_library(swiftStdlibStubs
                  OBJECT_LIBRARY TARGET_LIBRARY
                    ${swift_stubs_sources}
                    ${swift_stubs_objc_sources}
                    ${swift_stubs_unicode_normalization_sources}
                  C_COMPILE_FLAGS
                    ${swift_stubs_c_compile_flags}
                  LINK_FLAGS
                    ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
                  INSTALL_IN_COMPONENT
                    stdlib)

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set_property(SOURCE
                 SwiftNativeNSXXXBaseARC.m
               APPEND_STRING PROPERTY COMPILE_FLAGS
                 "-fobjc-arc")
endif()
