set(swift_runtime_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
set(swift_runtime_linker_flags ${SWIFT_RUNTIME_CORE_LINK_FLAGS})

if(SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT)
  list(APPEND swift_runtime_compile_flags
      "-DSWIFT_DARWIN_ENABLE_STABLE_ABI_BIT=1")
endif()

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

set(swift_runtime_objc_sources
    ErrorObject.mm
    SwiftObject.mm
    SwiftValue.mm
    ReflectionMirror.mm
    ObjCRuntimeGetImageNameFromClass.cpp
    "${SWIFT_SOURCE_DIR}/lib/Demangling/OldRemangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Remangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/TypeDecoder.cpp"
    )

set(swift_runtime_sources
    AnyHashableSupport.cpp
    Array.cpp
    Casting.cpp
    CompatibilityOverride.cpp
    CygwinPort.cpp
    Demangle.cpp
    Enum.cpp
    ErrorObjectConstants.cpp
    ErrorObjectNative.cpp
    Errors.cpp
    ErrorDefaultImpls.cpp
    Exclusivity.cpp
    ExistentialContainer.cpp
    Heap.cpp
    HeapObject.cpp
    ImageInspectionMachO.cpp
    ImageInspectionELF.cpp
    ImageInspectionCOFF.cpp
    KnownMetadata.cpp
    LLVMSupport.cpp
    Metadata.cpp
    MetadataLookup.cpp
    MutexPThread.cpp
    MutexWin32.cpp
    Once.cpp
    Portability.cpp
    ProtocolConformance.cpp
    RefCount.cpp
    RuntimeInvocationsTracking.cpp
    SwiftDtoa.cpp
    "${SWIFT_SOURCE_DIR}/lib/Demangling/OldDemangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Demangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/NodePrinter.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Context.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/ManglingUtils.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/Punycode.cpp")

# When we're building with assertions, include the demangle node dumper to aid
# in debugging.
if (LLVM_ENABLE_ASSERTIONS)
  list(APPEND swift_runtime_sources "${SWIFT_SOURCE_DIR}/lib/Demangling/NodeDumper.cpp")
endif(LLVM_ENABLE_ASSERTIONS)

# Acknowledge that the following sources are known.
set(LLVM_OPTIONAL_SOURCES
    MutexPThread.cpp
    MutexWin32.cpp
    CygwinPort.cpp
    ImageInspectionELF.cpp
    StaticBinaryELF.cpp
    SwiftRT-COFF.cpp
    SwiftRT-ELF.cpp
    ${swift_runtime_sources}
    ${swift_runtime_objc_sources}
    ${swift_runtime_leaks_sources})

set(swift_runtime_library_compile_flags ${swift_runtime_compile_flags})
list(APPEND swift_runtime_library_compile_flags -DswiftCore_EXPORTS)
list(APPEND swift_runtime_library_compile_flags -I${SWIFT_SOURCE_DIR}/include)

set(sdk "${SWIFT_HOST_VARIANT_SDK}")
if(SWIFT_BUILD_STATIC_STDLIB AND "${sdk}" STREQUAL "LINUX")
  list(REMOVE_ITEM swift_runtime_sources ImageInspectionELF.cpp)
  set(static_binary_lnk_file_list)
  string(TOLOWER "${sdk}" lowercase_sdk)

  # These two libraries are only used with the static swiftcore
  add_swift_library(swiftImageInspectionShared TARGET_LIBRARY STATIC
    ImageInspectionELF.cpp
    C_COMPILE_FLAGS ${swift_runtime_library_compile_flags}
    LINK_FLAGS ${swift_runtime_linker_flags}
    INSTALL_IN_COMPONENT stdlib)

  foreach(arch IN LISTS SWIFT_SDK_${sdk}_ARCHITECTURES)
    set(FragileSupportLibrary swiftImageInspectionShared-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch})
    set(LibraryLocation ${SWIFTSTATICLIB_DIR}/${lowercase_sdk}/${arch})
    add_custom_command_target(swift_image_inspection_${arch}_static
      COMMAND
        "${CMAKE_COMMAND}" -E copy $<TARGET_FILE:${FragileSupportLibrary}> ${LibraryLocation}
      OUTPUT
        "${LibraryLocation}/${CMAKE_STATIC_LIBRARY_PREFIX}swiftImageInspectionShared${CMAKE_STATIC_LIBRARY_SUFFIX}"
      DEPENDS
         ${FragileSupportLibrary})
    swift_install_in_component(stdlib
      FILES $<TARGET_FILE:${FragileSupportLibrary}>
      DESTINATION "lib/swift_static/${lowercase_sdk}/${arch}")
  endforeach()

  set(FragileSupportLibraryPrimary swiftImageInspectionShared-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${SWIFT_PRIMARY_VARIANT_ARCH})
  set(LibraryLocationPrimary ${SWIFTSTATICLIB_DIR}/${lowercase_sdk})
  add_custom_command_target(swift_image_inspection_static_primary_arch
      COMMAND
         "${CMAKE_COMMAND}" -E copy $<TARGET_FILE:${FragileSupportLibraryPrimary}> ${LibraryLocationPrimary}
      OUTPUT
      "${LibraryLocationPrimary}/${CMAKE_STATIC_LIBRARY_PREFIX}swiftImageInspectionShared${CMAKE_STATIC_LIBRARY_SUFFIX}"
      DEPENDS
         ${FragileSupportLibraryPrimary})
    swift_install_in_component(stdlib
      FILES $<TARGET_FILE:${FragileSupportLibraryPrimary}>
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
  foreach(arch IN LISTS SWIFT_SDK_LINUX_ARCHITECTURES)
    add_dependencies(static_binary_magic ${swift_image_inspection_${arch}_static})
  endforeach()
  add_dependencies(static_binary_magic ${swift_image_inspection_static_primary_arch})

  add_swift_library(swiftImageInspectionSharedObject OBJECT_LIBRARY TARGET_LIBRARY
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
set(COFF_SDKS)
foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  if("${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "ELF")
    list(APPEND ELFISH_SDKS ${sdk})
  elseif("${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "COFF")
    list(APPEND COFF_SDKS ${sdk})
  endif()
endforeach()

add_swift_library(swiftImageRegistrationObjectELF
                  OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
                  SwiftRT-ELF.cpp
                  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CORE_CXX_FLAGS}
                  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
                  TARGET_SDKS ${ELFISH_SDKS}
                  INSTALL_IN_COMPONENT none)
add_swift_library(swiftImageRegistrationObjectCOFF
                  OBJECT_LIBRARY IS_STDLIB IS_STDLIB_CORE
                  SwiftRT-COFF.cpp
                  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CORE_CXX_FLAGS}
                  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
                  TARGET_SDKS ${COFF_SDKS}
                  INSTALL_IN_COMPONENT none)

foreach(sdk ${SWIFT_CONFIGURED_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(arch_suffix "${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")

    if("${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "ELF" OR
       "${SWIFT_SDK_${sdk}_OBJECT_FORMAT}" STREQUAL "COFF")
      # TODO(compnerd) switch to the generator expression when cmake is upgraded
      # to a version which supports it.
      # set(swiftrtObject "$<TARGET_OBJECTS:swiftImageRegistrationObject${SWIFT_SDK_${sdk}_OBJECT_FORMAT}-${arch_suffix}>")
      set(swiftrtObject ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/swiftImageRegistrationObject${SWIFT_SDK_${sdk}_OBJECT_FORMAT}-${arch_suffix}.dir/SwiftRT-${SWIFT_SDK_${sdk}_OBJECT_FORMAT}.cpp${CMAKE_C_OUTPUT_EXTENSION})
      set(swiftrtPath "${SWIFTLIB_DIR}/${arch_subdir}/swiftrt${CMAKE_C_OUTPUT_EXTENSION}")

      add_custom_command_target(swiftImageRegistration-${arch_suffix}
                                COMMAND
                                  "${CMAKE_COMMAND}" -E copy "${swiftrtObject}" "${swiftrtPath}"
                                OUTPUT
                                  "${swiftrtPath}"
                                DEPENDS
                                  "${swiftrtObject}")
      swift_install_in_component(stdlib
                                 FILES
                                   "${swiftrtPath}"
                                 DESTINATION
                                   "lib/swift/${arch_subdir}")
      add_dependencies(swift-stdlib-${arch_suffix} ${swiftImageRegistration-${arch_suffix}})

      add_custom_target(swiftImageRegistration-${arch_suffix}
                        ALL DEPENDS
                          ${swiftImageRegistration-${arch_suffix}})
    endif()
  endforeach()
endforeach()

