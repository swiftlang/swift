set(swift_stubs_sources
    Assert.cpp
    CommandLine.cpp
    GlobalObjects.cpp
    LibcShims.cpp
    Stubs.cpp
    UnicodeExtendedGraphemeClusters.cpp.gyb)
set(swift_stubs_objc_sources
    Availability.mm
    FoundationHelpers.mm
    OptionalBridgingHelper.mm
    Reflection.mm
    SwiftNativeNSXXXBase.mm.gyb)
set(swift_stubs_unicode_normalization_sources
    UnicodeNormalization.cpp)
set(LLVM_OPTIONAL_SOURCES
    ${swift_stubs_objc_sources}
    ${swift_stubs_unicode_normalization_sources})

# ICU isn't required on Darwin, but is on every other platform.
# Now in case we're cross-compiling from Darwin for another platform,
# the find_package should still be executed.
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(icu_required "")
else()
  set(icu_required "REQUIRED")
endif()

find_package(ICU ${icu_required} COMPONENTS uc)
set(ICU_UC_LIBRARY "")

set(swift_stubs_c_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
list(APPEND swift_stubs_c_compile_flags -DswiftCore_EXPORTS)

add_swift_target_library(swiftStdlibStubs OBJECT_LIBRARY TARGET_LIBRARY
  ${swift_stubs_sources}
  ${swift_stubs_objc_sources}
  ${swift_stubs_unicode_normalization_sources}
  C_COMPILE_FLAGS ${swift_stubs_c_compile_flags}
  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
  INSTALL_IN_COMPONENT stdlib)

