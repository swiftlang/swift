set(swift_stubs_sources
    Assert.cpp
    GlobalObjects.cpp
    LibcShims.cpp
    Random.cpp
    Stubs.cpp
    ThreadLocalStorage.cpp
    MathStubs.cpp
    Unicode/UnicodeData.cpp
    Unicode/UnicodeGrapheme.cpp
    Unicode/UnicodeNormalization.cpp
    Unicode/UnicodeScalarProps.cpp
    Unicode/UnicodeWord.cpp
)
set(swift_stubs_objc_sources
    Availability.mm
    FoundationHelpers.mm
    OptionalBridgingHelper.mm
    Reflection.mm
    SwiftNativeNSObject.mm
    SwiftNativeNSXXXBaseARC.m)
set(swift_stubs_gyb_sources
    SwiftNativeNSXXXBase.mm.gyb)
set(LLVM_OPTIONAL_SOURCES
    ${swift_stubs_objc_sources}
    ${swift_stubs_gyb_sources})

set(swift_stubs_c_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
list(APPEND swift_stubs_c_compile_flags -DswiftCore_EXPORTS)
list(APPEND swift_stubs_c_compile_flags -I${SWIFT_SOURCE_DIR}/include -I${SWIFT_SOURCE_DIR}/stdlib/public)

add_swift_target_library(swiftStdlibStubs
                  OBJECT_LIBRARY
                    ${swift_stubs_sources}
                    ${swift_stubs_objc_sources}
                  GYB_SOURCES
                    ${swift_stubs_gyb_sources}
                  C_COMPILE_FLAGS
                    ${swift_stubs_c_compile_flags}
                  LINK_FLAGS
                    ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
                  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
                  INSTALL_IN_COMPONENT
                    stdlib)

if("${SWIFT_PRIMARY_VARIANT_SDK}" IN_LIST SWIFT_DARWIN_PLATFORMS)
  set_property(SOURCE
                 SwiftNativeNSXXXBaseARC.m
               APPEND_STRING PROPERTY COMPILE_FLAGS
                 "-fobjc-arc")
endif()

# Embedded Swift Unicode library
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_target(embedded-unicode ALL)

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)

    if("${mod}" MATCHES "-windows-msvc$")
      continue()
    endif()

    if (SWIFT_HOST_VARIANT STREQUAL "linux")
      set(extra_c_compile_flags -ffreestanding)
    elseif (SWIFT_HOST_VARIANT STREQUAL "macosx")
      set(extra_c_compile_flags -D__MACH__ -D__APPLE__ -ffreestanding)
    endif()
    
    set(SWIFT_SDK_embedded_ARCH_${mod}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${mod}_TRIPLE "${triple}")

    add_swift_target_library_single(
      embedded-unicode-${mod}
      swiftUnicodeDataTables
      STATIC
      IS_FRAGILE

      Unicode/UnicodeData.cpp
      Unicode/UnicodeGrapheme.cpp
      Unicode/UnicodeNormalization.cpp
      Unicode/UnicodeScalarProps.cpp
      Unicode/UnicodeWord.cpp

      C_COMPILE_FLAGS ${extra_c_compile_flags}
      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${mod}"
      DEPENDS embedded-stdlib-${mod}
      INSTALL_IN_COMPONENT stdlib
      )
    swift_install_in_component(
      TARGETS embedded-unicode-${mod}
      DESTINATION "lib/swift/embedded/${mod}"
      COMPONENT "stdlib"
      )
    swift_install_in_component(
      FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftUnicodeDataTables.a"
      DESTINATION "lib/swift/embedded/${mod}/"
      COMPONENT "stdlib"
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      )
    set_property(TARGET embedded-unicode-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

    add_dependencies(embedded-unicode embedded-unicode-${mod})
  endforeach()
endif()
