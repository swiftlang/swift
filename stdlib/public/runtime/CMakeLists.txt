set(swift_runtime_compile_flags ${SWIFT_CORE_CXX_FLAGS})

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

# Acknowledge that the following sources are known.
set(LLVM_OPTIONAL_SOURCES
    Remangle.cpp
    swift_sections.S)

set(swift_runtime_objc_sources)
set(swift_runtime_unicode_normalization_sources)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_runtime_objc_sources
      ErrorObject.mm
      SwiftObject.mm
      Remangle.cpp
      Reflection.mm)
else()
endif()

add_swift_library(swiftRuntime IS_STDLIB IS_STDLIB_CORE
  Casting.cpp
  Demangle.cpp
  Enum.cpp
  ErrorObject.cpp
  Errors.cpp
  Heap.cpp
  HeapObject.cpp
  KnownMetadata.cpp
  Metadata.cpp
  MetadataLookup.cpp
  Once.cpp
  ProtocolConformance.cpp
  Reflection.cpp
  SwiftObject.cpp
  ${swift_runtime_objc_sources}
  ${swift_runtime_leaks_sources}
  C_COMPILE_FLAGS ${swift_runtime_compile_flags}
  INSTALL_IN_COMPONENT stdlib)

set(object_target_list)
foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  if("${sdk}" STREQUAL "LINUX" OR "${sdk}" STREQUAL "FREEBSD")
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")

      set(section_magic_begin_name "section_magic_begin_${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}")
      set(section_magic_end_name "section_magic_end_${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}")

      add_library(${section_magic_begin_name} STATIC swift_sections.S)
      set_target_properties(${section_magic_begin_name} PROPERTIES COMPILE_FLAGS "-DSWIFT_BEGIN")

      add_library(${section_magic_end_name} STATIC swift_sections.S)
      set_target_properties(${section_magic_end_name} PROPERTIES COMPILE_FLAGS "-DSWIFT_END")

      add_custom_command_target(${section_magic_begin_name}_begin
        OUTPUT  "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
        COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${section_magic_begin_name}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}" "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
        DEPENDS ${section_magic_begin_name})

      add_custom_command_target(${section_magic_begin_name}_end
        OUTPUT  "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
        COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${section_magic_end_name}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}" "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
        DEPENDS ${section_magic_end_name})

      list(APPEND object_target_list "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o" "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o")

      swift_install_in_component(compiler
          FILES "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o" "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
          DESTINATION "lib/swift/${arch_subdir}")

    endforeach()
  endif()
endforeach()

add_custom_target(section_magic ALL DEPENDS ${object_target_list})

