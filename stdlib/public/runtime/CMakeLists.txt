set(swift_runtime_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
set(swift_runtime_linker_flags ${SWIFT_RUNTIME_CORE_LINK_FLAGS})

if(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=1")
endif()

if(SWIFT_RUNTIME_CRASH_REPORTER_CLIENT)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()

set(swift_runtime_leaks_sources)
if(SWIFT_RUNTIME_ENABLE_LEAK_CHECKER)
  list(APPEND swift_runtime_compile_flags
       "-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER=1")
  set(swift_runtime_leaks_sources Leaks.mm)
endif()

list(APPEND swift_runtime_compile_flags
     "-D__SWIFT_CURRENT_DYLIB=swiftCore")

set(swift_runtime_objc_sources)
set(swift_runtime_unicode_normalization_sources)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_runtime_objc_sources
      ErrorObject.mm
      SwiftObject.mm
      SwiftValue.mm
      Remangle.cpp
      Reflection.mm)
else()
endif()

set(swift_runtime_sources
    AnyHashableSupport.cpp
    Casting.cpp
    CygwinPort.cpp
    Demangle.cpp
    Enum.cpp
    ErrorObjectConstants.cpp
    ErrorObjectNative.cpp
    Errors.cpp
    Heap.cpp
    HeapObject.cpp
    KnownMetadata.cpp
    Metadata.cpp
    MetadataLookup.cpp
    MutexPThread.cpp
    MutexWin32.cpp
    Once.cpp
    Portability.cpp
    ProtocolConformance.cpp
    ReflectionNative.cpp
    RuntimeEntrySymbols.cpp
    SwiftObjectNative.cpp)

# Acknowledge that the following sources are known.
set(LLVM_OPTIONAL_SOURCES
    Remangle.cpp
    swift_sections.S
    MutexPThread.cpp
    MutexWin32.cpp
    CygwinPort.cpp
    ${swift_runtime_sources}
    ${swift_runtime_objc_sources}
    ${swift_runtime_leaks_sources})

add_swift_library(swiftRuntime OBJECT_LIBRARY TARGET_LIBRARY
  ${swift_runtime_sources}
  ${swift_runtime_objc_sources}
  ${swift_runtime_leaks_sources}
  C_COMPILE_FLAGS ${swift_runtime_compile_flags} -DswiftCore_EXPORTS
  LINK_FLAGS ${swift_runtime_linker_flags}
  INSTALL_IN_COMPONENT never_install)

set(ELFISH_SDKS)
foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  if("${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "ELF")
    list(APPEND ELFISH_SDKS "${sdk}")
  endif()
endforeach()

add_swift_library(section_magic_begin OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
    swift_sections.S
    C_COMPILE_FLAGS ${swift_runtime_compile_flags} "-DSWIFT_BEGIN"
    TARGET_SDKS "${ELFISH_SDKS}"
    LINK_FLAGS ${swift_runtime_linker_flags}
    INSTALL_IN_COMPONENT never_install)
add_swift_library(section_magic_end OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
    swift_sections.S
    C_COMPILE_FLAGS ${swift_runtime_compile_flags} "-DSWIFT_END"
    LINK_FLAGS ${swift_runtime_linker_flags}
    TARGET_SDKS "${ELFISH_SDKS}"
    INSTALL_IN_COMPONENT never_install)

set(object_target_list)
foreach(sdk ${ELFISH_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(arch_suffix "${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")

    set(section_magic_begin_name "section_magic_begin-${arch_suffix}")
    set(section_magic_end_name "section_magic_end-${arch_suffix}")

    add_custom_command_target(section_magic_${arch_suffix}_objects
      COMMAND
          "${CMAKE_COMMAND}" -E copy
          "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${section_magic_begin_name}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}"
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
      COMMAND
          "${CMAKE_COMMAND}" -E copy
          "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${section_magic_end_name}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}"
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
      OUTPUT
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
      DEPENDS
          ${section_magic_begin_name}
          ${section_magic_end_name})

    list(APPEND object_target_list "${section_magic_${arch_suffix}_objects}")

    swift_install_in_component(stdlib
        FILES "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o" "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
        DESTINATION "lib/swift/${arch_subdir}")
  endforeach()
endforeach()

add_custom_target(section_magic ALL DEPENDS ${object_target_list})
