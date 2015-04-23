set(swift_runtime_compile_flags)

if(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=1")
endif()

if(SWIFT_HAVE_CRASH_REPORTER_CLIENT)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()

set(swift_runtime_leaks_sources)
if(SWIFT_RUNTIME_ENABLE_LEAK_CHECKER)
  list(APPEND swift_runtime_compile_flags
       "-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER=1")
  set(swift_runtime_leaks_sources Leaks.mm)
endif()

set(swift_runtime_dtrace_sources)
if (SWIFT_RUNTIME_ENABLE_DTRACE)
  set(swift_runtime_dtrace_sources SwiftRuntimeDTraceProbes.d)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_RUNTIME_ENABLE_DTRACE=1")
endif()

if (SWIFT_EXPERIMENTAL_ENABLE_GUARANTEED_SELF)
  list(APPEND swift_runtime_compile_flags "-DSWIFT_RUNTIME_ENABLE_GUARANTEED_SELF")
endif()

set(swift_runtime_objc_sources)
set(swift_runtime_unicode_normalization_sources)
set(swift_runtime_link_libraries)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_runtime_objc_sources
      Availability.mm
      ErrorObject.mm
      SwiftObject.mm
      SwiftNativeNSXXXBase.mm.gyb
      Reflection.mm)
  set(LLVM_OPTIONAL_SOURCES
      UnicodeNormalization.cpp)
else()
  find_package(ICU REQUIRED COMPONENTS uc i18n)
  set(swift_runtime_unicode_normalization_sources
      UnicodeNormalization.cpp)
  set(swift_runtime_link_libraries
      ${ICU_UC_LIBRARY} ${ICU_I18N_LIBRARY})
  include_directories(
      ${ICU_UC_INCLUDE_DIR} ${ICU_I18N_INCLUDE_DIR})
endif()


add_swift_library(swiftRuntime IS_STDLIB IS_STDLIB_CORE
  Casting.cpp
  Demangle.cpp
  ErrorObject.cpp
  GlobalObjects.cpp
  HeapObject.cpp
  KnownMetadata.cpp
  Metadata.cpp
  Reflection.cpp
  ShimsChecker.cpp
  Stubs.cpp
  SwiftObject.cpp
  Enum.cpp
  Once.cpp
  Heap.cpp
  Errors.cpp
  UnicodeExtendedGraphemeClusters.cpp.gyb
  ${swift_runtime_objc_sources}
  ${swift_runtime_dtrace_sources}
  ${swift_runtime_leaks_sources}
  ${swift_runtime_unicode_normalization_sources}
  C_COMPILE_FLAGS ${swift_runtime_compile_flags}
  LINK_LIBRARIES ${swift_runtime_link_libraries}
  INSTALL_IN_COMPONENT stdlib)

foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  if("${sdk}" STREQUAL "LINUX")
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")

      # FIXME: We will need a different linker script for 32-bit builds.
      configure_file(
          "swift.ld" "${SWIFTLIB_DIR}/${arch_subdir}/swift.ld" COPYONLY)

      swift_install_in_component(compiler
          FILES "swift.ld"
          DESTINATION "lib/swift/${arch_subdir}")

    endforeach()
  endif()
endforeach()

