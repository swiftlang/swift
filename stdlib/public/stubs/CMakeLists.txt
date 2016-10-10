set(swift_stubs_sources
    Assert.cpp
    CommandLine.cpp
    GlobalObjects.cpp
    LibcShims.cpp
    Stubs.cpp
    UnicodeExtendedGraphemeClusters.cpp.gyb)
set(swift_stubs_objc_sources
    Availability.mm
    DispatchShims.mm
    FoundationHelpers.mm
    OptionalBridgingHelper.mm
    Reflection.mm
    SwiftNativeNSXXXBase.mm.gyb)
set(swift_stubs_unicode_normalization_sources
    UnicodeNormalization.cpp)
set(LLVM_OPTIONAL_SOURCES
    ${swift_stubs_objc_sources}
    ${swift_stubs_unicode_normalization_sources})

set(swift_stubs_c_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
list(APPEND swift_stubs_c_compile_flags -DswiftCore_EXPORTS)

add_swift_library(swiftStdlibStubs OBJECT_LIBRARY TARGET_LIBRARY
  ${swift_stubs_sources}
  ${swift_stubs_objc_sources}
  C_COMPILE_FLAGS ${swift_stubs_c_compile_flags}
  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
  TARGET_SDKS ALL_APPLE_PLATFORMS
  INSTALL_IN_COMPONENT stdlib)

add_swift_library(swiftStdlibStubs OBJECT_LIBRARY TARGET_LIBRARY
  ${swift_stubs_sources}
  ${swift_stubs_unicode_normalization_sources}
  C_COMPILE_FLAGS ${swift_stubs_c_compile_flags}
  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
  TARGET_SDKS ANDROID CYGWIN FREEBSD LINUX
  INSTALL_IN_COMPONENT stdlib)

