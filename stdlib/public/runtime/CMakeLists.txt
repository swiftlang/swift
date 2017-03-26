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

if(SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS)
  list(APPEND swift_runtime_compile_flags
    "-DSWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS=1")
endif()

set(section_magic_compile_flags ${swift_runtime_compile_flags})

list(APPEND swift_runtime_compile_flags
     "-D__SWIFT_CURRENT_DYLIB=swiftCore")

set(swift_runtime_objc_sources
    ErrorObject.mm
    SwiftObject.mm
    SwiftValue.mm
    Reflection.mm
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Remangler.cpp")

set(swift_runtime_sources
    AnyHashableSupport.cpp
    Casting.cpp
    CygwinPort.cpp
    Demangle.cpp
    Enum.cpp
    ErrorObjectConstants.cpp
    ErrorObjectNative.cpp
    Errors.cpp
    ErrorDefaultImpls.cpp
    Heap.cpp
    HeapObject.cpp
    ImageInspectionMachO.cpp
    ImageInspectionELF.cpp
    ImageInspectionWin32.cpp
    KnownMetadata.cpp
    Metadata.cpp
    MetadataLookup.cpp
    MutexPThread.cpp
    MutexWin32.cpp
    Once.cpp
    Portability.cpp
    ProtocolConformance.cpp
    RefCount.cpp
    RuntimeEntrySymbols.cpp
    "${SWIFT_SOURCE_DIR}/lib/Demangling/OldDemangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Demangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/NodePrinter.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Context.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/ManglingUtils.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Punycode.cpp")

# Acknowledge that the following sources are known.
set(LLVM_OPTIONAL_SOURCES
    swift_sections.S
    MutexPThread.cpp
    MutexWin32.cpp
    CygwinPort.cpp
    ImageInspectionInit.cpp
    ImageInspectionDL.cpp
    ImageInspectionELF.cpp
    ImageInspectionStatic.cpp
    StaticBinaryELF.cpp
    ${swift_runtime_sources}
    ${swift_runtime_objc_sources}
    ${swift_runtime_leaks_sources})

set(swift_runtime_library_compile_flags ${swift_runtime_compile_flags})
list(APPEND swift_runtime_library_compile_flags -DswiftCore_EXPORTS)

set(sdk "${SWIFT_HOST_VARIANT_SDK}")
if(SWIFT_BUILD_STATIC_STDLIB AND "${sdk}" STREQUAL "LINUX")
  list(REMOVE_ITEM swift_runtime_sources ImageInspectionELF.cpp)
  set(static_binary_lnk_file_list)
  string(TOLOWER "${sdk}" lowercase_sdk)

  # These two libraries are only used with the static swiftcore
  add_library(swiftImageInspectionStatic STATIC
              ImageInspectionStatic.cpp
              StaticBinaryELF.cpp)
  set_target_properties(swiftImageInspectionStatic PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${SWIFTSTATICLIB_DIR}/${lowercase_sdk}")

  add_library(swiftImageInspectionShared STATIC ImageInspectionELF.cpp)
  set_target_properties(swiftImageInspectionShared PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${SWIFTSTATICLIB_DIR}/${lowercase_sdk}")

  swift_install_in_component(stdlib
    TARGETS swiftImageInspectionStatic swiftImageInspectionShared
    DESTINATION "lib/swift_static/${lowercase_sdk}")

  # Generate the static-executable-args.lnk file used for ELF systems (eg linux)
  set(linkfile "${lowercase_sdk}/static-executable-args.lnk")
  add_custom_command_target(swift_static_binary_${sdk}_args
    COMMAND
      "${CMAKE_COMMAND}" -E copy
      "${SWIFT_SOURCE_DIR}/utils/static-executable-args.lnk"
      "${SWIFTSTATICLIB_DIR}/${linkfile}"
    OUTPUT
      "${SWIFTSTATICLIB_DIR}/${linkfile}"
    DEPENDS
      "${SWIFT_SOURCE_DIR}/utils/static-executable-args.lnk")

  list(APPEND static_binary_lnk_file_list ${swift_static_binary_${sdk}_args})
  swift_install_in_component(stdlib
    FILES "${SWIFTSTATICLIB_DIR}/${linkfile}"
    DESTINATION "lib/swift_static/${lowercase_sdk}")
  add_custom_target(static_binary_magic ALL DEPENDS ${static_binary_lnk_file_list})

  add_swift_library(swiftImageInspectionShared OBJECT_LIBRARY TARGET_LIBRARY
    ImageInspectionELF.cpp
    C_COMPILE_FLAGS ${swift_runtime_library_compile_flags}
    LINK_FLAGS ${swift_runtime_linker_flags}
    INSTALL_IN_COMPONENT never_install)
endif()

add_swift_library(swiftRuntime OBJECT_LIBRARY TARGET_LIBRARY
  ${swift_runtime_sources}
  ${swift_runtime_objc_sources}
  ${swift_runtime_leaks_sources}
  C_COMPILE_FLAGS ${swift_runtime_library_compile_flags}
  LINK_FLAGS ${swift_runtime_linker_flags}
  INSTALL_IN_COMPONENT never_install)

set(ELFISH_SDKS)
foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  if("${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "ELF")
    list(APPEND ELFISH_SDKS "${sdk}")
  endif()
endforeach()

add_swift_library(section_magic_loader OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
    ImageInspectionInit.cpp
    C_COMPILE_FLAGS ${section_magic_compile_flags}
    TARGET_SDKS "${ELFISH_SDKS}"
    LINK_FLAGS ${swift_runtime_linker_flags}
    INSTALL_IN_COMPONENT never_install)
add_swift_library(section_magic_begin OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
    swift_sections.S
    C_COMPILE_FLAGS ${section_magic_compile_flags} "-DSWIFT_BEGIN"
    TARGET_SDKS "${ELFISH_SDKS}"
    LINK_FLAGS ${swift_runtime_linker_flags}
    INSTALL_IN_COMPONENT never_install)
add_swift_library(section_magic_end OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
    swift_sections.S
    C_COMPILE_FLAGS ${section_magic_compile_flags} "-DSWIFT_END"
    LINK_FLAGS ${swift_runtime_linker_flags}
    TARGET_SDKS "${ELFISH_SDKS}"
    INSTALL_IN_COMPONENT never_install)

set(object_target_list)
foreach(sdk ${ELFISH_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(arch_suffix "${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")

    set(section_magic_loader_obj "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/section_magic_loader-${arch_suffix}.dir/ImageInspectionInit.cpp${CMAKE_C_OUTPUT_EXTENSION}")
    set(section_magic_begin_obj "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/section_magic_begin-${arch_suffix}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}")
    set(section_magic_end_obj "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/section_magic_end-${arch_suffix}.dir/swift_sections.S${CMAKE_C_OUTPUT_EXTENSION}")

    set(ld_EXECUTABLE ${CMAKE_LINKER})
    if(SWIFT_SDK_${sdk}_ARCH_${arch}_LINKER)
      set(ld_EXECUTABLE ${SWIFT_SDK_${sdk}_ARCH_${arch}_LINKER})
    endif()

    add_custom_command_target(section_magic_${arch_suffix}_begin_object
      COMMAND
          # Merge ImageInspectionInit.o + swift_sections.S(BEGIN) => swift_begin.o
          ${ld_EXECUTABLE} -r -o "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
          "${section_magic_begin_obj}" "${section_magic_loader_obj}"
      OUTPUT
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
      DEPENDS
          "${section_magic_begin_obj}"
          "${section_magic_loader_obj}")

    add_custom_command_target(section_magic_${arch_suffix}_end_object
      COMMAND
          "${CMAKE_COMMAND}" -E copy
          "${section_magic_end_obj}"
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
      OUTPUT
          "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
      DEPENDS
          "${section_magic_end_obj}")

    list(APPEND object_target_list
        "${section_magic_${arch_suffix}_begin_object}"
        "${section_magic_${arch_suffix}_end_object}")

    swift_install_in_component(stdlib
        FILES
            "${SWIFTLIB_DIR}/${arch_subdir}/swift_begin.o"
            "${SWIFTLIB_DIR}/${arch_subdir}/swift_end.o"
        DESTINATION
            "lib/swift/${arch_subdir}")

    if(SWIFT_BUILD_STATIC_STDLIB)
      # Static lib versions of swift_begin.o and swift_end.o
      add_custom_command_target(static_section_magic_${arch_suffix}_begin_object
        COMMAND
            "${CMAKE_COMMAND}" -E copy
            "${section_magic_begin_obj}"
            "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_begin.o"
        OUTPUT
            "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_begin.o"
        DEPENDS
            "${section_magic_begin_obj}")

      add_custom_command_target(static_section_magic_${arch_suffix}_end_object
        COMMAND
            "${CMAKE_COMMAND}" -E copy
            "${section_magic_end_obj}"
            "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_end.o"
        OUTPUT
            "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_end.o"
        DEPENDS
            "${section_magic_end_obj}")

      list(APPEND object_target_list
            "${static_section_magic_${arch_suffix}_begin_object}"
            "${static_section_magic_${arch_suffix}_end_object}")

      swift_install_in_component(stdlib
          FILES
              "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_begin.o"
              "${SWIFTSTATICLIB_DIR}/${arch_subdir}/swift_end.o"
          DESTINATION
              "lib/swift_static/${arch_subdir}")
    endif()

  endforeach()
endforeach()

add_custom_target(section_magic ALL DEPENDS ${object_target_list})
