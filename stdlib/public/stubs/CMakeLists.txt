set(swift_stubs_objc_sources)
set(swift_stubs_unicode_normalization_sources)
set(swift_stubs_link_libraries)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_stubs_objc_sources
      Availability.mm
      FoundationHelpers.mm
      SwiftNativeNSXXXBase.mm.gyb)
  set(LLVM_OPTIONAL_SOURCES
      UnicodeNormalization.cpp)
else()
  find_package(ICU REQUIRED COMPONENTS uc i18n)
  set(swift_stubs_unicode_normalization_sources
      UnicodeNormalization.cpp)
  set(swift_stubs_link_libraries
      ${ICU_UC_LIBRARY} ${ICU_I18N_LIBRARY})
  include_directories(
      ${ICU_UC_INCLUDE_DIR} ${ICU_I18N_INCLUDE_DIR})
endif()

add_swift_library(swiftStdlibStubs IS_STDLIB IS_STDLIB_CORE
  Assert.cpp
  GlobalObjects.cpp
  LibcShims.cpp
  Stubs.cpp
  UnicodeExtendedGraphemeClusters.cpp.gyb
  ${swift_stubs_objc_sources}
  ${swift_stubs_unicode_normalization_sources}
  C_COMPILE_FLAGS ${SWIFT_CORE_CXX_FLAGS}
  LINK_LIBRARIES ${swift_stubs_link_libraries}
  INSTALL_IN_COMPONENT stdlib)

