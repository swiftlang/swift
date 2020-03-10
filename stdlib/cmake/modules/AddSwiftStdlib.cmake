
include(AddSwift)
include(SwiftSource)

# Add a universal binary target created from the output of the given
# set of targets by running 'lipo'.
#
# Usage:
#   _add_swift_lipo_target(
#     sdk                 # The name of the SDK the target was created for.
#                         # Examples include "OSX", "IOS", "ANDROID", etc.
#     target              # The name of the target to create
#     output              # The file to be created by this target
#     source_targets...   # The source targets whose outputs will be
#                         # lipo'd into the output.
#   )
function(_add_swift_lipo_target)
  cmake_parse_arguments(
    LIPO                # prefix
    "CODESIGN"          # options
    "SDK;TARGET;OUTPUT" # single-value args
    ""                  # multi-value args
    ${ARGN})

  precondition(LIPO_SDK MESSAGE "sdk is required")
  precondition(LIPO_TARGET MESSAGE "target is required")
  precondition(LIPO_OUTPUT MESSAGE "output is required")
  precondition(LIPO_UNPARSED_ARGUMENTS MESSAGE "one or more inputs are required")

  set(source_targets ${LIPO_UNPARSED_ARGUMENTS})

  # Gather the source binaries.
  set(source_binaries)
  foreach(source_target ${source_targets})
    list(APPEND source_binaries $<TARGET_FILE:${source_target}>)
  endforeach()

  if(${LIPO_SDK} IN_LIST SWIFT_APPLE_PLATFORMS)
    if(LIPO_CODESIGN)
      set(codesign_command COMMAND "codesign" "-f" "-s" "-" "${LIPO_OUTPUT}")
    endif()
    # Use lipo to create the final binary.
    add_custom_command_target(unused_var
        COMMAND "${SWIFT_LIPO}" "-create" "-output" "${LIPO_OUTPUT}" ${source_binaries}
        ${codesign_command}
        CUSTOM_TARGET_NAME "${LIPO_TARGET}"
        OUTPUT "${LIPO_OUTPUT}"
        DEPENDS ${source_targets})
  else()
    # We don't know how to create fat binaries for other platforms.
    add_custom_command_target(unused_var
        COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${source_binaries}" "${LIPO_OUTPUT}"
        CUSTOM_TARGET_NAME "${LIPO_TARGET}"
        OUTPUT "${LIPO_OUTPUT}"
        DEPENDS ${source_targets})
  endif()
endfunction()
 
# Add a single variant of a new Swift library.
#
# Usage:
#   _add_swift_target_library_single(
#     target
#     name
#     [MODULE_TARGETS]
#     [SHARED]
#     [STATIC]
#     [SDK sdk]
#     [ARCHITECTURE architecture]
#     [DEPENDS dep1 ...]
#     [LINK_LIBRARIES dep1 ...]
#     [FRAMEWORK_DEPENDS dep1 ...]
#     [FRAMEWORK_DEPENDS_WEAK dep1 ...]
#     [LLVM_LINK_COMPONENTS comp1 ...]
#     [C_COMPILE_FLAGS flag1...]
#     [SWIFT_COMPILE_FLAGS flag1...]
#     [LINK_FLAGS flag1...]
#     [FILE_DEPENDS target1 ...]
#     [DONT_EMBED_BITCODE]
#     [IS_STDLIB]
#     [IS_STDLIB_CORE]
#     [IS_SDK_OVERLAY]
#     INSTALL_IN_COMPONENT comp
#     MACCATALYST_BUILD_FLAVOR flavor
#     source1 [source2 source3 ...])
#
# target
#   Name of the target (e.g., swiftParse-IOS-armv7).
#
# name
#   Name of the library (e.g., swiftParse).
#
# MODULE_TARGETS
#   Names of the module target (e.g., swiftParse-swiftmodule-IOS-armv7).
#
# SHARED
#   Build a shared library.
#
# STATIC
#   Build a static library.
#
# SDK sdk
#   SDK to build for.
#
# ARCHITECTURE
#   Architecture to build for.
#
# DEPENDS
#   Targets that this library depends on.
#
# LINK_LIBRARIES
#   Libraries this library depends on.
#
# FRAMEWORK_DEPENDS
#   System frameworks this library depends on.
#
# FRAMEWORK_DEPENDS_WEAK
#   System frameworks this library depends on that should be weakly-linked.
#
# LLVM_LINK_COMPONENTS
#   LLVM components this library depends on.
#
# C_COMPILE_FLAGS
#   Extra compile flags (C, C++, ObjC).
#
# SWIFT_COMPILE_FLAGS
#   Extra compile flags (Swift).
#
# LINK_FLAGS
#   Extra linker flags.
#
# FILE_DEPENDS
#   Additional files this library depends on.
#
# DONT_EMBED_BITCODE
#   Don't embed LLVM bitcode in this target, even if it is enabled globally.
#
# IS_STDLIB
#   Install library dylib and swift module files to lib/swift.
#
# IS_STDLIB_CORE
#   Compile as the standard library core.
#
# IS_SDK_OVERLAY
#   Treat the library as a part of the Swift SDK overlay.
#
# INSTALL_IN_COMPONENT comp
#   The Swift installation component that this library belongs to.
#
# MACCATALYST_BUILD_FLAVOR
#   Possible values are 'ios-like', 'macos-like', 'zippered', 'unzippered-twin'
#
# source1 ...
#   Sources to add into this library
function(_add_swift_target_library_single target name)
  set(SWIFTLIB_SINGLE_options
        DONT_EMBED_BITCODE
        IS_SDK_OVERLAY
        IS_STDLIB
        IS_STDLIB_CORE
        NOSWIFTRT
        OBJECT_LIBRARY
        SHARED
        STATIC
        TARGET_LIBRARY
        INSTALL_WITH_SHARED)
  set(SWIFTLIB_SINGLE_single_parameter_options
        ARCHITECTURE
        DEPLOYMENT_VERSION_IOS
        DEPLOYMENT_VERSION_OSX
        DEPLOYMENT_VERSION_TVOS
        DEPLOYMENT_VERSION_WATCHOS
        INSTALL_IN_COMPONENT
        DARWIN_INSTALL_NAME_DIR
        SDK
        DEPLOYMENT_VERSION_MACCATALYST
        MACCATALYST_BUILD_FLAVOR)
  set(SWIFTLIB_SINGLE_multiple_parameter_options
        C_COMPILE_FLAGS
        DEPENDS
        FILE_DEPENDS
        FRAMEWORK_DEPENDS
        FRAMEWORK_DEPENDS_WEAK
        GYB_SOURCES
        INCORPORATE_OBJECT_LIBRARIES
        INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY
        LINK_FLAGS
        LINK_LIBRARIES
        LLVM_LINK_COMPONENTS
        PRIVATE_LINK_LIBRARIES
        SWIFT_COMPILE_FLAGS
        MODULE_TARGETS)

  cmake_parse_arguments(SWIFTLIB_SINGLE
                        "${SWIFTLIB_SINGLE_options}"
                        "${SWIFTLIB_SINGLE_single_parameter_options}"
                        "${SWIFTLIB_SINGLE_multiple_parameter_options}"
                        ${ARGN})

  # Determine macCatalyst build flavor
  get_maccatalyst_build_flavor(maccatalyst_build_flavor
    "${SWIFTLIB_SINGLE_SDK}" "${SWIFTLIB_SINGLE_MACCATALYST_BUILD_FLAVOR}")

  set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_UNPARSED_ARGUMENTS})

  translate_flags(SWIFTLIB_SINGLE "${SWIFTLIB_SINGLE_options}")

  # Check arguments.
  precondition(SWIFTLIB_SINGLE_SDK MESSAGE "Should specify an SDK")
  precondition(SWIFTLIB_SINGLE_ARCHITECTURE MESSAGE "Should specify an architecture")
  precondition(SWIFTLIB_SINGLE_INSTALL_IN_COMPONENT MESSAGE "INSTALL_IN_COMPONENT is required")

  if(NOT SWIFTLIB_SINGLE_SHARED AND
     NOT SWIFTLIB_SINGLE_STATIC AND
     NOT SWIFTLIB_SINGLE_OBJECT_LIBRARY)
    message(FATAL_ERROR
        "Either SHARED, STATIC, or OBJECT_LIBRARY must be specified")
  endif()

  # Determine the subdirectory where this library will be installed.
  set(SWIFTLIB_SINGLE_SUBDIR
      "${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}/${SWIFTLIB_SINGLE_ARCHITECTURE}")

  # macCatalyst ios-like builds are installed in the maccatalyst/x86_64 directory
  if(maccatalyst_build_flavor STREQUAL "ios-like")
    set(SWIFTLIB_SINGLE_SUBDIR
        "${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}/${SWIFTLIB_SINGLE_ARCHITECTURE}")
  endif()

  # Include LLVM Bitcode slices for iOS, Watch OS, and Apple TV OS device libraries.
  set(embed_bitcode_arg)
  if(SWIFT_EMBED_BITCODE_SECTION AND NOT SWIFTLIB_SINGLE_DONT_EMBED_BITCODE)
    if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "IOS" OR "${SWIFTLIB_SINGLE_SDK}" STREQUAL "TVOS" OR "${SWIFTLIB_SINGLE_SDK}" STREQUAL "WATCHOS")
      list(APPEND SWIFTLIB_SINGLE_C_COMPILE_FLAGS "-fembed-bitcode")
      set(embed_bitcode_arg EMBED_BITCODE)
    endif()
  endif()

  if(XCODE)
    string(REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list(GET split_path -1 dir)
    file(GLOB_RECURSE SWIFTLIB_SINGLE_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def
      ${CMAKE_CURRENT_SOURCE_DIR}/*.def)

    file(GLOB_RECURSE SWIFTLIB_SINGLE_TDS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td)

    set_source_files_properties(${SWIFTLIB_SINGLE_HEADERS} ${SWIFTLIB_SINGLE_TDS}
      PROPERTIES
      HEADER_FILE_ONLY true)
    source_group("TableGen descriptions" FILES ${SWIFTLIB_SINGLE_TDS})

    set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_SOURCES} ${SWIFTLIB_SINGLE_HEADERS} ${SWIFTLIB_SINGLE_TDS})
  endif()

  if(MODULE)
    set(libkind MODULE)
  elseif(SWIFTLIB_SINGLE_OBJECT_LIBRARY)
    set(libkind OBJECT)
  # If both SHARED and STATIC are specified, we add the SHARED library first.
  # The STATIC library is handled further below.
  elseif(SWIFTLIB_SINGLE_SHARED)
    set(libkind SHARED)
  elseif(SWIFTLIB_SINGLE_STATIC)
    set(libkind STATIC)
  else()
    message(FATAL_ERROR
        "Either SHARED, STATIC, or OBJECT_LIBRARY must be specified")
  endif()

  if(SWIFTLIB_SINGLE_GYB_SOURCES)
    handle_gyb_sources(
        gyb_dependency_targets
        SWIFTLIB_SINGLE_GYB_SOURCES
        "${SWIFTLIB_SINGLE_ARCHITECTURE}")
    set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_SOURCES}
      ${SWIFTLIB_SINGLE_GYB_SOURCES})
  endif()

  # Remove the "swift" prefix from the name to determine the module name.
  if(SWIFTLIB_IS_STDLIB_CORE)
    set(module_name "Swift")
  else()
    string(REPLACE swift "" module_name "${name}")
  endif()

  if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "WINDOWS")
    if(NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
      swift_windows_get_sdk_vfs_overlay(SWIFTLIB_SINGLE_VFS_OVERLAY)
      list(APPEND SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS
        -Xcc;-Xclang;-Xcc;-ivfsoverlay;-Xcc;-Xclang;-Xcc;${SWIFTLIB_SINGLE_VFS_OVERLAY})
    endif()
    swift_windows_include_for_arch(${SWIFTLIB_SINGLE_ARCHITECTURE} SWIFTLIB_INCLUDE)
    foreach(directory ${SWIFTLIB_INCLUDE})
      list(APPEND SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS -Xcc;-isystem;-Xcc;${directory})
    endforeach()
    if("${SWIFTLIB_SINGLE_ARCHITECTURE}" MATCHES arm)
      list(APPEND SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS -Xcc;-D_ARM_WINAPI_PARTITION_DESKTOP_SDK_AVAILABLE)
    endif()
    list(APPEND SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS
      -libc;${SWIFT_STDLIB_MSVC_RUNTIME_LIBRARY})
  endif()

  # FIXME: don't actually depend on the libraries in SWIFTLIB_SINGLE_LINK_LIBRARIES,
  # just any swiftmodule files that are associated with them.
  handle_swift_sources(
      swift_object_dependency_target
      swift_module_dependency_target
      swift_sib_dependency_target
      swift_sibopt_dependency_target
      swift_sibgen_dependency_target
      SWIFTLIB_SINGLE_SOURCES
      SWIFTLIB_SINGLE_EXTERNAL_SOURCES ${name}
      DEPENDS
        ${gyb_dependency_targets}
        ${SWIFTLIB_SINGLE_DEPENDS}
        ${SWIFTLIB_SINGLE_FILE_DEPENDS}
        ${SWIFTLIB_SINGLE_LINK_LIBRARIES}
      SDK ${SWIFTLIB_SINGLE_SDK}
      ARCHITECTURE ${SWIFTLIB_SINGLE_ARCHITECTURE}
      MODULE_NAME ${module_name}
      COMPILE_FLAGS ${SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS}
      ${SWIFTLIB_SINGLE_IS_STDLIB_keyword}
      ${SWIFTLIB_SINGLE_IS_STDLIB_CORE_keyword}
      ${SWIFTLIB_SINGLE_IS_SDK_OVERLAY_keyword}
      ${embed_bitcode_arg}
      INSTALL_IN_COMPONENT "${SWIFTLIB_SINGLE_INSTALL_IN_COMPONENT}"
      MACCATALYST_BUILD_FLAVOR "${SWIFTLIB_SINGLE_MACCATALYST_BUILD_FLAVOR}")
  add_swift_source_group("${SWIFTLIB_SINGLE_EXTERNAL_SOURCES}")

  # If there were any swift sources, then a .swiftmodule may have been created.
  # If that is the case, then add a target which is an alias of the module files.
  set(VARIANT_SUFFIX "-${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}-${SWIFTLIB_SINGLE_ARCHITECTURE}")
  if(maccatalyst_build_flavor STREQUAL "ios-like")
    set(VARIANT_SUFFIX "-${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}-${SWIFTLIB_SINGLE_ARCHITECTURE}")
  endif()

  if(NOT "${SWIFTLIB_SINGLE_MODULE_TARGETS}" STREQUAL "" AND NOT "${swift_module_dependency_target}" STREQUAL "")
    foreach(module_target ${SWIFTLIB_SINGLE_MODULE_TARGETS})
      add_custom_target("${module_target}"
        DEPENDS ${swift_module_dependency_target})
      set_target_properties("${module_target}" PROPERTIES
        FOLDER "Swift libraries/Modules")
    endforeach()
  endif()

  # For standalone overlay builds to work
  if(NOT BUILD_STANDALONE)
    if (EXISTS swift_sib_dependency_target AND NOT "${swift_sib_dependency_target}" STREQUAL "")
      add_dependencies(swift-stdlib${VARIANT_SUFFIX}-sib ${swift_sib_dependency_target})
    endif()

    if (EXISTS swift_sibopt_dependency_target AND NOT "${swift_sibopt_dependency_target}" STREQUAL "")
      add_dependencies(swift-stdlib${VARIANT_SUFFIX}-sibopt ${swift_sibopt_dependency_target})
    endif()

    if (EXISTS swift_sibgen_dependency_target AND NOT "${swift_sibgen_dependency_target}" STREQUAL "")
      add_dependencies(swift-stdlib${VARIANT_SUFFIX}-sibgen ${swift_sibgen_dependency_target})
    endif()
  endif()

  # Only build the modules for any arch listed in the *_MODULE_ARCHITECTURES.
  if(SWIFTLIB_SINGLE_SDK IN_LIST SWIFT_APPLE_PLATFORMS
      AND SWIFTLIB_SINGLE_ARCHITECTURE IN_LIST SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_MODULE_ARCHITECTURES)
    # Create dummy target to hook up the module target dependency.
    add_custom_target("${target}"
      DEPENDS
        "${swift_module_dependency_target}")

    return()
  endif()

  set(SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS)
  foreach(object_library ${SWIFTLIB_SINGLE_INCORPORATE_OBJECT_LIBRARIES})
    list(APPEND SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS
        $<TARGET_OBJECTS:${object_library}${VARIANT_SUFFIX}>)
  endforeach()

  set(SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS_SHARED_ONLY)
  foreach(object_library ${SWIFTLIB_SINGLE_INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY})
    list(APPEND SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS_SHARED_ONLY
        $<TARGET_OBJECTS:${object_library}${VARIANT_SUFFIX}>)
  endforeach()

  set(SWIFTLIB_SINGLE_XCODE_WORKAROUND_SOURCES)
  if(XCODE AND SWIFTLIB_SINGLE_TARGET_LIBRARY)
    set(SWIFTLIB_SINGLE_XCODE_WORKAROUND_SOURCES
        # Note: the dummy.cpp source file provides no definitions. However,
        # it forces Xcode to properly link the static library.
        ${SWIFT_SOURCE_DIR}/cmake/dummy.cpp)
  endif()

  set(INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS ${SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS})
  if(${libkind} STREQUAL "SHARED")
    list(APPEND INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS
         ${SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS_SHARED_ONLY})
  endif()

  add_library("${target}" ${libkind}
              ${SWIFTLIB_SINGLE_SOURCES}
              ${SWIFTLIB_SINGLE_EXTERNAL_SOURCES}
              ${INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS}
              ${SWIFTLIB_SINGLE_XCODE_WORKAROUND_SOURCES})
  if(("${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_OBJECT_FORMAT}" STREQUAL "ELF" OR
      "${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_OBJECT_FORMAT}" STREQUAL "COFF") AND
     SWIFTLIB_SINGLE_TARGET_LIBRARY)
    if("${libkind}" STREQUAL "SHARED" AND NOT SWIFTLIB_SINGLE_NOSWIFTRT)
      # TODO(compnerd) switch to the generator expression when cmake is upgraded
      # to a version which supports it.
      # target_sources(${target}
      #                PRIVATE
      #                  $<TARGET_OBJECTS:swiftImageRegistrationObject${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_OBJECT_FORMAT}-${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}-${SWIFTLIB_SINGLE_ARCHITECTURE}>)
      if(SWIFTLIB_SINGLE_SDK STREQUAL WINDOWS)
        set(extension .obj)
      else()
        set(extension .o)
      endif()
      target_sources(${target}
                     PRIVATE
                       "${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}/swiftrt${extension}")
      set_source_files_properties("${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}/swiftrt${extension}"
                                  PROPERTIES
                                    GENERATED 1)
    endif()
  endif()
  _set_target_prefix_and_suffix("${target}" "${libkind}" "${SWIFTLIB_SINGLE_SDK}")

  if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "WINDOWS")
    swift_windows_include_for_arch(${SWIFTLIB_SINGLE_ARCHITECTURE} SWIFTLIB_INCLUDE)
    target_include_directories("${target}" SYSTEM PRIVATE ${SWIFTLIB_INCLUDE})
    set_target_properties(${target}
                          PROPERTIES
                            CXX_STANDARD 14)
  endif()

  if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "WINDOWS" AND NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    if("${libkind}" STREQUAL "SHARED")
      # Each dll has an associated .lib (import library); since we may be
      # building on a non-DLL platform (not windows), create an imported target
      # for the library which created implicitly by the dll.
      add_custom_command_target(${target}_IMPORT_LIBRARY
                                OUTPUT "${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}/${name}.lib"
                                DEPENDS "${target}")
      add_library(${target}_IMPLIB SHARED IMPORTED GLOBAL)
      set_property(TARGET "${target}_IMPLIB" PROPERTY
          IMPORTED_LOCATION "${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}/${name}.lib")
      add_dependencies(${target}_IMPLIB ${${target}_IMPORT_LIBRARY})
    endif()
    set_property(TARGET "${target}" PROPERTY NO_SONAME ON)
  endif()

  llvm_update_compile_flags(${target})

  set_output_directory(${target}
      BINARY_DIR ${SWIFT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  if(MODULE)
    set_target_properties("${target}" PROPERTIES
        PREFIX ""
        SUFFIX ${LLVM_PLUGIN_EXT})
  endif()

  if(SWIFTLIB_SINGLE_TARGET_LIBRARY)
    # Install runtime libraries to lib/swift instead of lib. This works around
    # the fact that -isysroot prevents linking to libraries in the system
    # /usr/lib if Swift is installed in /usr.
    set_target_properties("${target}" PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR})
    if(SWIFTLIB_SINGLE_SDK STREQUAL WINDOWS AND SWIFTLIB_SINGLE_IS_STDLIB_CORE
        AND libkind STREQUAL SHARED)
      add_custom_command(TARGET ${target} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${target}> ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR})
    endif()

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      escape_path_for_xcode("${config}" "${SWIFTLIB_DIR}" config_lib_dir)
      set_target_properties(${target} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()
  endif()

  if(SWIFTLIB_SINGLE_SDK IN_LIST SWIFT_APPLE_PLATFORMS)
    set(install_name_dir "@rpath")

    if(SWIFTLIB_SINGLE_IS_STDLIB)
      set(install_name_dir "${SWIFT_DARWIN_STDLIB_INSTALL_NAME_DIR}")

      # iOS-like overlays are installed in a separate directory so that
      # unzippered twins do not conflict.
      if(maccatalyst_build_flavor STREQUAL "ios-like"
          AND DEFINED SWIFT_DARWIN_MACCATALYST_STDLIB_INSTALL_NAME_DIR)
        set(install_name_dir "${SWIFT_DARWIN_MACCATALYST_STDLIB_INSTALL_NAME_DIR}")
      endif()
    endif()

    # Always use @rpath for XCTest
    if(module_name STREQUAL "XCTest")
      set(install_name_dir "@rpath")
    endif()

    if(SWIFTLIB_SINGLE_DARWIN_INSTALL_NAME_DIR)
      set(install_name_dir "${SWIFTLIB_SINGLE_DARWIN_INSTALL_NAME_DIR}")
    endif()

    set_target_properties("${target}"
      PROPERTIES
      INSTALL_NAME_DIR "${install_name_dir}")
  elseif("${SWIFTLIB_SINGLE_SDK}" STREQUAL "LINUX")
    set_target_properties("${target}"
      PROPERTIES
      INSTALL_RPATH "$ORIGIN:/usr/lib/swift/linux")
  elseif("${SWIFTLIB_SINGLE_SDK}" STREQUAL "CYGWIN")
    set_target_properties("${target}"
      PROPERTIES
      INSTALL_RPATH "$ORIGIN:/usr/lib/swift/cygwin")
  elseif("${SWIFTLIB_SINGLE_SDK}" STREQUAL "ANDROID")
    # CMake generates an incorrect rule `$SONAME_FLAG $INSTALLNAME_DIR$SONAME`
    # for an Android cross-build from a macOS host. Construct the proper linker
    # flags manually in add_swift_target_library instead, see there with
    # variable `swiftlib_link_flags_all`.
    if(SWIFTLIB_SINGLE_TARGET_LIBRARY)
      set_target_properties("${target}" PROPERTIES NO_SONAME TRUE)
    endif()
    # Only set the install RPATH if cross-compiling the host tools, in which
    # case both the NDK and Sysroot paths must be set.
    if(NOT "${SWIFT_ANDROID_NDK_PATH}" STREQUAL "" AND
       NOT "${SWIFT_ANDROID_NATIVE_SYSROOT}" STREQUAL "")
      set_target_properties("${target}"
        PROPERTIES
        INSTALL_RPATH "$ORIGIN")
    endif()
  endif()

  set_target_properties("${target}" PROPERTIES BUILD_WITH_INSTALL_RPATH YES)
  set_target_properties("${target}" PROPERTIES FOLDER "Swift libraries")

  # Configure the static library target.
  # Set compile and link flags for the non-static target.
  # Do these LAST.
  set(target_static)
  if(SWIFTLIB_SINGLE_IS_STDLIB AND SWIFTLIB_SINGLE_STATIC)
    set(target_static "${target}-static")

    # We have already compiled Swift sources.  Link everything into a static
    # library.
    add_library(${target_static} STATIC
        ${SWIFTLIB_SINGLE_SOURCES}
        ${SWIFTLIB_INCORPORATED_OBJECT_LIBRARIES_EXPRESSIONS}
        ${SWIFTLIB_SINGLE_XCODE_WORKAROUND_SOURCES})

    set_output_directory(${target_static}
        BINARY_DIR ${SWIFT_RUNTIME_OUTPUT_INTDIR}
        LIBRARY_DIR ${SWIFT_LIBRARY_OUTPUT_INTDIR})

    if(SWIFTLIB_INSTALL_WITH_SHARED)
      set(swift_lib_dir ${SWIFTLIB_DIR})
    else()
      set(swift_lib_dir ${SWIFTSTATICLIB_DIR})
    endif()

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      escape_path_for_xcode(
          "${config}" "${swift_lib_dir}" config_lib_dir)
      set_target_properties(${target_static} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()

    set_target_properties(${target_static} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${swift_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${swift_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
  endif()

  set_target_properties(${target}
      PROPERTIES
      # Library name (without the variant information)
      OUTPUT_NAME ${name})
  if(target_static)
    set_target_properties(${target_static}
        PROPERTIES
        OUTPUT_NAME ${name})
  endif()

  # Don't build standard libraries by default.  We will enable building
  # standard libraries that the user requested; the rest can be built on-demand.
  if(SWIFTLIB_SINGLE_TARGET_LIBRARY)
    foreach(t "${target}" ${target_static})
      set_target_properties(${t} PROPERTIES EXCLUDE_FROM_ALL TRUE)
    endforeach()
  endif()

  # Handle linking and dependencies.
  add_dependencies_multiple_targets(
      TARGETS "${target}" ${target_static}
      DEPENDS
        ${SWIFTLIB_SINGLE_DEPENDS}
        ${gyb_dependency_targets}
        "${swift_object_dependency_target}"
        "${swift_module_dependency_target}"
        ${LLVM_COMMON_DEPENDS})

  if("${libkind}" STREQUAL "SHARED")
    target_link_libraries("${target}" PRIVATE ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
  elseif("${libkind}" STREQUAL "OBJECT")
    precondition_list_empty(
        "${SWIFTLIB_SINGLE_LINK_LIBRARIES}"
        "OBJECT_LIBRARY may not link to anything")
  else()
    target_link_libraries("${target}" INTERFACE ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
  endif()

  # Don't add the icucore target.
  set(SWIFTLIB_SINGLE_LINK_LIBRARIES_WITHOUT_ICU)
  foreach(item ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
    if(NOT "${item}" STREQUAL "icucore")
      list(APPEND SWIFTLIB_SINGLE_LINK_LIBRARIES_WITHOUT_ICU "${item}")
    endif()
  endforeach()

  if(target_static)
    _list_add_string_suffix(
        "${SWIFTLIB_SINGLE_LINK_LIBRARIES_WITHOUT_ICU}"
        "-static"
        target_static_depends)
    # FIXME: should this be target_link_libraries?
    add_dependencies_multiple_targets(
        TARGETS "${target_static}"
        DEPENDS ${target_static_depends})
  endif()

  # Link against system frameworks.
  foreach(FRAMEWORK ${SWIFTLIB_SINGLE_FRAMEWORK_DEPENDS})
    foreach(t "${target}" ${target_static})
      target_link_libraries("${t}" PUBLIC "-framework ${FRAMEWORK}")
    endforeach()
  endforeach()
  foreach(FRAMEWORK ${SWIFTLIB_SINGLE_FRAMEWORK_DEPENDS_WEAK})
    foreach(t "${target}" ${target_static})
      target_link_libraries("${t}" PUBLIC "-weak_framework ${FRAMEWORK}")
    endforeach()
  endforeach()

  if(NOT SWIFTLIB_SINGLE_TARGET_LIBRARY)
    # Call llvm_config() only for libraries that are part of the compiler.
    swift_common_llvm_config("${target}" ${SWIFTLIB_SINGLE_LLVM_LINK_COMPONENTS})
  endif()

  # Collect compile and link flags for the static and non-static targets.
  # Don't set PROPERTY COMPILE_FLAGS or LINK_FLAGS directly.
  set(c_compile_flags ${SWIFTLIB_SINGLE_C_COMPILE_FLAGS})
  set(link_flags ${SWIFTLIB_SINGLE_LINK_FLAGS})

  set(library_search_subdir "${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}")
  set(library_search_directories
      "${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}"
      "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFTLIB_SINGLE_SUBDIR}"
      "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}")

  # In certain cases when building, the environment variable SDKROOT is set to override
  # where the sdk root is located in the system. If that environment variable has been
  # set by the user, respect it and add the specified SDKROOT directory to the
  # library_search_directories so we are able to link against those libraries
  if(DEFINED ENV{SDKROOT} AND EXISTS "$ENV{SDKROOT}/usr/lib/swift")
      list(APPEND library_search_directories "$ENV{SDKROOT}/usr/lib/swift")
  endif()

  # Add variant-specific flags.
  if(SWIFTLIB_SINGLE_TARGET_LIBRARY)
    set(build_type "${SWIFT_STDLIB_BUILD_TYPE}")
    set(enable_assertions "${SWIFT_STDLIB_ASSERTIONS}")
  else()
    set(build_type "${CMAKE_BUILD_TYPE}")
    set(enable_assertions "${LLVM_ENABLE_ASSERTIONS}")
    set(analyze_code_coverage "${SWIFT_ANALYZE_CODE_COVERAGE}")
  endif()

  if (NOT SWIFTLIB_SINGLE_TARGET_LIBRARY)
    set(lto_type "${SWIFT_TOOLS_ENABLE_LTO}")
  endif()

  _add_variant_c_compile_flags(
    SDK "${SWIFTLIB_SINGLE_SDK}"
    ARCH "${SWIFTLIB_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    ENABLE_LTO "${lto_type}"
    DEPLOYMENT_VERSION_OSX "${SWIFTLIB_DEPLOYMENT_VERSION_OSX}"
    DEPLOYMENT_VERSION_MACCATALYST "${SWIFTLIB_DEPLOYMENT_VERSION_MACCATALYST}"
    DEPLOYMENT_VERSION_IOS "${SWIFTLIB_DEPLOYMENT_VERSION_IOS}"
    DEPLOYMENT_VERSION_TVOS "${SWIFTLIB_DEPLOYMENT_VERSION_TVOS}"
    DEPLOYMENT_VERSION_WATCHOS "${SWIFTLIB_DEPLOYMENT_VERSION_WATCHOS}"
    RESULT_VAR_NAME c_compile_flags
    MACCATALYST_BUILD_FLAVOR "${SWIFTLIB_SINGLE_MACCATALYST_BUILD_FLAVOR}"
    )

  if(SWIFTLIB_IS_STDLIB)
    # We don't ever want to link against the ABI-breakage checking symbols
    # in the standard library, runtime, or overlays because they only rely
    # on the header parts of LLVM's ADT.
    list(APPEND c_compile_flags
      "-DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1")
  endif()

  if(SWIFTLIB_SINGLE_SDK STREQUAL WINDOWS)
    if(libkind STREQUAL SHARED)
      list(APPEND c_compile_flags -D_WINDLL)
    endif()
  endif()
  _add_variant_link_flags(
    SDK "${SWIFTLIB_SINGLE_SDK}"
    ARCH "${SWIFTLIB_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    ENABLE_LTO "${lto_type}"
    LTO_OBJECT_NAME "${target}-${SWIFTLIB_SINGLE_SDK}-${SWIFTLIB_SINGLE_ARCHITECTURE}"
    DEPLOYMENT_VERSION_OSX "${SWIFTLIB_DEPLOYMENT_VERSION_OSX}"
    DEPLOYMENT_VERSION_MACCATALYST "${SWIFTLIB_DEPLOYMENT_VERSION_MACCATALYST}"
    DEPLOYMENT_VERSION_IOS "${SWIFTLIB_DEPLOYMENT_VERSION_IOS}"
    DEPLOYMENT_VERSION_TVOS "${SWIFTLIB_DEPLOYMENT_VERSION_TVOS}"
    DEPLOYMENT_VERSION_WATCHOS "${SWIFTLIB_DEPLOYMENT_VERSION_WATCHOS}"
    RESULT_VAR_NAME link_flags
    LINK_LIBRARIES_VAR_NAME link_libraries
    LIBRARY_SEARCH_DIRECTORIES_VAR_NAME library_search_directories
    MACCATALYST_BUILD_FLAVOR "${SWIFTLIB_SINGLE_MACCATALYST_BUILD_FLAVOR}"
      )

  # Configure plist creation for OS X.
  set(PLIST_INFO_PLIST "Info.plist" CACHE STRING "Plist name")
  if("${SWIFTLIB_SINGLE_SDK}" IN_LIST SWIFT_APPLE_PLATFORMS AND SWIFTLIB_SINGLE_IS_STDLIB)
    set(PLIST_INFO_NAME ${name})
    set(PLIST_INFO_UTI "com.apple.dt.runtime.${name}")
    set(PLIST_INFO_VERSION "${SWIFT_VERSION}")
    if (SWIFT_COMPILER_VERSION)
      set(PLIST_INFO_BUILD_VERSION
        "${SWIFT_COMPILER_VERSION}")
    endif()

    set(PLIST_INFO_PLIST_OUT "${PLIST_INFO_PLIST}")
    list(APPEND link_flags
         "-Wl,-sectcreate,__TEXT,__info_plist,${CMAKE_CURRENT_BINARY_DIR}/${PLIST_INFO_PLIST_OUT}")
    configure_file(
        "${SWIFT_SOURCE_DIR}/stdlib/${PLIST_INFO_PLIST}.in"
        "${PLIST_INFO_PLIST_OUT}"
        @ONLY
        NEWLINE_STYLE UNIX)

    # If Application Extensions are enabled, pass the linker flag marking
    # the dylib as safe.
    if (CXX_SUPPORTS_FAPPLICATION_EXTENSION AND (NOT DISABLE_APPLICATION_EXTENSION))
      list(APPEND link_flags "-Wl,-application_extension")
    endif()

    set(PLIST_INFO_UTI)
    set(PLIST_INFO_NAME)
    set(PLIST_INFO_VERSION)
    set(PLIST_INFO_BUILD_VERSION)
  endif()

  # Set compilation and link flags.
  if(SWIFTLIB_SINGLE_SDK STREQUAL WINDOWS)
    swift_windows_include_for_arch(${SWIFTLIB_SINGLE_ARCHITECTURE}
      ${SWIFTLIB_SINGLE_ARCHITECTURE}_INCLUDE)
    target_include_directories(${target} SYSTEM PRIVATE
      ${${SWIFTLIB_SINGLE_ARCHITECTURE}_INCLUDE})

    if(NOT ${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
      swift_windows_get_sdk_vfs_overlay(SWIFTLIB_SINGLE_VFS_OVERLAY)
      target_compile_options(${target} PRIVATE
        "SHELL:-Xclang -ivfsoverlay -Xclang ${SWIFTLIB_SINGLE_VFS_OVERLAY}")

      # MSVC doesn't support -Xclang. We don't need to manually specify
      # the dependent libraries as `cl` does so.
      target_compile_options(${target} PRIVATE
        "SHELL:-Xclang --dependent-lib=oldnames"
        # TODO(compnerd) handle /MT, /MTd
        "SHELL:-Xclang --dependent-lib=msvcrt$<$<CONFIG:Debug>:d>")
    endif()
  endif()
  target_include_directories(${target} SYSTEM PRIVATE
    ${SWIFT_${SWIFTLIB_SINGLE_SDK}_${SWIFTLIB_SINGLE_ARCHITECTURE}_ICU_UC_INCLUDE}
    ${SWIFT_${SWIFTLIB_SINGLE_SDK}_${SWIFTLIB_SINGLE_ARCHITECTURE}_ICU_I18N_INCLUDE})
  target_compile_options(${target} PRIVATE
    ${c_compile_flags})
  target_link_options(${target} PRIVATE
    ${link_flags})
  if(${SWIFTLIB_SINGLE_SDK} IN_LIST SWIFT_APPLE_PLATFORMS)
    target_link_options(${target} PRIVATE
      "LINKER:-compatibility_version,1")
    if(SWIFT_COMPILER_VERSION)
      target_link_options(${target} PRIVATE
        "LINKER:-current_version,${SWIFT_COMPILER_VERSION}")
    endif()
    # Include LLVM Bitcode slices for iOS, Watch OS, and Apple TV OS device libraries.
    if(SWIFT_EMBED_BITCODE_SECTION AND NOT SWIFTLIB_SINGLE_DONT_EMBED_BITCODE)
      if(${SWIFTLIB_SINGLE_SDK} MATCHES "(I|TV|WATCH)OS")
        # The two branches of this if statement accomplish the same end result
        # We are simply accounting for the fact that on CMake < 3.16
        # using a generator expression to
        # specify a LINKER: argument does not work,
        # since that seems not to allow the LINKER: prefix to be
        # evaluated (i.e. it will be added as-is to the linker parameters)
        if(CMAKE_VERSION VERSION_LESS 3.16)
          target_link_options(${target} PRIVATE
            "LINKER:-bitcode_bundle"
            "LINKER:-lto_library,${LLVM_LIBRARY_DIR}/libLTO.dylib")

          if(SWIFT_EMBED_BITCODE_SECTION_HIDE_SYMBOLS)
            target_link_options(${target} PRIVATE
              "LINKER:-bitcode_hide_symbols")
          endif()
        else()
          target_link_options(${target} PRIVATE
            "LINKER:-bitcode_bundle"
            $<$<BOOL:SWIFT_EMBED_BITCODE_SECTION_HIDE_SYMBOLS>:"LINKER:-bitcode_hide_symbols">
            "LINKER:-lto_library,${LLVM_LIBRARY_DIR}/libLTO.dylib")
        endif()
      endif()
    endif()
  endif()
  target_link_libraries(${target} PRIVATE
    ${link_libraries})
  target_link_directories(${target} PRIVATE
    ${library_search_directories})

  # Adjust the linked libraries for windows targets.  On Windows, the link is
  # performed against the import library, and the runtime uses the dll.  Not
  # doing so will result in incorrect symbol resolution and linkage.  We created
  # import library targets when the library was added.  Use that to adjust the
  # link libraries.
  if(SWIFTLIB_SINGLE_SDK STREQUAL WINDOWS AND NOT CMAKE_SYSTEM_NAME STREQUAL Windows)
    foreach(library_list LINK_LIBRARIES PRIVATE_LINK_LIBRARIES)
      set(import_libraries)
      foreach(library ${SWIFTLIB_SINGLE_${library_list}})
        # Ensure that the library is a target.  If an absolute path was given,
        # then we do not have an import library associated with it.  This occurs
        # primarily with ICU (which will be an import library).  Import
        # libraries are only associated with shared libraries, so add an
        # additional check for that as well.
        set(import_library ${library})
        if(TARGET ${library})
          get_target_property(type ${library} TYPE)
          if(${type} STREQUAL "SHARED_LIBRARY")
            set(import_library ${library}_IMPLIB)
          endif()
        endif()
        list(APPEND import_libraries ${import_library})
      endforeach()
      set(SWIFTLIB_SINGLE_${library_list} ${import_libraries})
    endforeach()
  endif()

  if("${libkind}" STREQUAL "OBJECT")
    precondition_list_empty(
        "${SWIFTLIB_SINGLE_PRIVATE_LINK_LIBRARIES}"
        "OBJECT_LIBRARY may not link to anything")
  else()
    target_link_libraries("${target}" PRIVATE
        ${SWIFTLIB_SINGLE_PRIVATE_LINK_LIBRARIES})
  endif()

  # NOTE(compnerd) use the C linker language to invoke `clang` rather than
  # `clang++` as we explicitly link against the C++ runtime.  We were previously
  # actually passing `-nostdlib++` to avoid the C++ runtime linkage.
  if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "ANDROID")
    set_property(TARGET "${target}" PROPERTY
      LINKER_LANGUAGE "C")
  else()
    set_property(TARGET "${target}" PROPERTY
      LINKER_LANGUAGE "CXX")
  endif()

  if(target_static)
    target_compile_options(${target_static} PRIVATE
      ${c_compile_flags})
    # FIXME: The fallback paths here are going to be dynamic libraries.

    if(SWIFTLIB_INSTALL_WITH_SHARED)
      set(search_base_dir ${SWIFTLIB_DIR})
    else()
      set(search_base_dir ${SWIFTSTATICLIB_DIR})
    endif()
    set(library_search_directories
        "${search_base_dir}/${SWIFTLIB_SINGLE_SUBDIR}"
        "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFTLIB_SINGLE_SUBDIR}"
        "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}")
    target_link_directories(${target_static} PRIVATE
      ${library_search_directories})
    target_link_libraries("${target_static}" PRIVATE
        ${SWIFTLIB_SINGLE_PRIVATE_LINK_LIBRARIES})
  endif()

  # Do not add code here.
endfunction()

# Add a new Swift target library.
#
# NOTE: This has not had the swift host code debrided from it yet. That will be
# in a forthcoming commit.
#
# Usage:
#   add_swift_target_library(name
#     [SHARED]
#     [STATIC]
#     [DEPENDS dep1 ...]
#     [LINK_LIBRARIES dep1 ...]
#     [SWIFT_MODULE_DEPENDS dep1 ...]
#     [FRAMEWORK_DEPENDS dep1 ...]
#     [FRAMEWORK_DEPENDS_WEAK dep1 ...]
#     [LLVM_LINK_COMPONENTS comp1 ...]
#     [FILE_DEPENDS target1 ...]
#     [TARGET_SDKS sdk1...]
#     [C_COMPILE_FLAGS flag1...]
#     [SWIFT_COMPILE_FLAGS flag1...]
#     [LINK_FLAGS flag1...]
#     [DONT_EMBED_BITCODE]
#     [INSTALL]
#     [IS_STDLIB]
#     [IS_STDLIB_CORE]
#     [INSTALL_WITH_SHARED]
#     INSTALL_IN_COMPONENT comp
#     DEPLOYMENT_VERSION_OSX version
#     DEPLOYMENT_VERSION_MACCATALYST version
#     DEPLOYMENT_VERSION_IOS version
#     DEPLOYMENT_VERSION_TVOS version
#     DEPLOYMENT_VERSION_WATCHOS version
#     MACCATALYST_BUILD_FLAVOR flavor
#     source1 [source2 source3 ...])
#
# name
#   Name of the library (e.g., swiftParse).
#
# SHARED
#   Build a shared library.
#
# STATIC
#   Build a static library.
#
# DEPENDS
#   Targets that this library depends on.
#
# LINK_LIBRARIES
#   Libraries this library depends on.
#
# SWIFT_MODULE_DEPENDS
#   Swift modules this library depends on.
#
# SWIFT_MODULE_DEPENDS_OSX
#   Swift modules this library depends on when built for OS X.
#
# SWIFT_MODULE_DEPENDS_MACCATALYST
#   Zippered Swift modules this library depends on when built for macCatalyst.
#   For example, Foundation.
#
# SWIFT_MODULE_DEPENDS_MACCATALYST_UNZIPPERED
#   Unzippered Swift modules this library depends on when built for macCatalyst.
#   For example, UIKit
#
# SWIFT_MODULE_DEPENDS_IOS
#   Swift modules this library depends on when built for iOS.
#
# SWIFT_MODULE_DEPENDS_TVOS
#   Swift modules this library depends on when built for tvOS.
#
# SWIFT_MODULE_DEPENDS_WATCHOS
#   Swift modules this library depends on when built for watchOS.
#
# SWIFT_MODULE_DEPENDS_FREEBSD
#   Swift modules this library depends on when built for FreeBSD.
#
# SWIFT_MODULE_DEPENDS_LINUX
#   Swift modules this library depends on when built for Linux.
#
# SWIFT_MODULE_DEPENDS_CYGWIN
#   Swift modules this library depends on when built for Cygwin.
#
# SWIFT_MODULE_DEPENDS_HAIKU
#   Swift modules this library depends on when built for Haiku.
#
# FRAMEWORK_DEPENDS
#   System frameworks this library depends on.
#
# FRAMEWORK_DEPENDS_WEAK
#   System frameworks this library depends on that should be weak-linked
#
# LLVM_LINK_COMPONENTS
#   LLVM components this library depends on.
#
# FILE_DEPENDS
#   Additional files this library depends on.
#
# TARGET_SDKS
#   The set of SDKs in which this library is included. If empty, the library
#   is included in all SDKs.
#
# C_COMPILE_FLAGS
#   Extra compiler flags (C, C++, ObjC).
#
# SWIFT_COMPILE_FLAGS
#   Extra compiler flags (Swift).
#
# LINK_FLAGS
#   Extra linker flags.
#
# DONT_EMBED_BITCODE
#   Don't embed LLVM bitcode in this target, even if it is enabled globally.
#
# IS_STDLIB
#   Treat the library as a part of the Swift standard library.
#
# IS_STDLIB_CORE
#   Compile as the Swift standard library core.
#
# IS_SDK_OVERLAY
#   Treat the library as a part of the Swift SDK overlay.
#
# INSTALL_IN_COMPONENT comp
#   The Swift installation component that this library belongs to.
#
# DEPLOYMENT_VERSION_OSX
#   The minimum deployment version to build for if this is an OSX library.
#
# DEPLOYMENT_VERSION_MACCATALYST
#   The minimum deployment version to build for if this is an macCatalyst library.
#
# DEPLOYMENT_VERSION_IOS
#   The minimum deployment version to build for if this is an iOS library.
#
# DEPLOYMENT_VERSION_TVOS
#   The minimum deployment version to build for if this is an TVOS library.
#
# DEPLOYMENT_VERSION_WATCHOS
#   The minimum deployment version to build for if this is an WATCHOS library.
#
# INSTALL_WITH_SHARED
#   Install a static library target alongside shared libraries
#
# MACCATALYST_BUILD_FLAVOR
#   Possible values are 'ios-like', 'macos-like', 'zippered', 'unzippered-twin'
#   Presence of a build flavor requires SWIFT_MODULE_DEPENDS_MACCATALYST to be
#   defined and have values.
#
# source1 ...
#   Sources to add into this library.
function(add_swift_target_library name)
  set(SWIFTLIB_options
        DONT_EMBED_BITCODE
        HAS_SWIFT_CONTENT
        IS_SDK_OVERLAY
        IS_STDLIB
        IS_STDLIB_CORE
        NOSWIFTRT
        OBJECT_LIBRARY
        SHARED
        STATIC
        INSTALL_WITH_SHARED)
  set(SWIFTLIB_single_parameter_options
        DEPLOYMENT_VERSION_IOS
        DEPLOYMENT_VERSION_OSX
        DEPLOYMENT_VERSION_TVOS
        DEPLOYMENT_VERSION_WATCHOS
        INSTALL_IN_COMPONENT
        DARWIN_INSTALL_NAME_DIR
        DEPLOYMENT_VERSION_MACCATALYST
        MACCATALYST_BUILD_FLAVOR)
  set(SWIFTLIB_multiple_parameter_options
        C_COMPILE_FLAGS
        DEPENDS
        FILE_DEPENDS
        FRAMEWORK_DEPENDS
        FRAMEWORK_DEPENDS_IOS_TVOS
        FRAMEWORK_DEPENDS_OSX
        FRAMEWORK_DEPENDS_WEAK
        GYB_SOURCES
        INCORPORATE_OBJECT_LIBRARIES
        INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY
        LINK_FLAGS
        LINK_LIBRARIES
        LLVM_LINK_COMPONENTS
        PRIVATE_LINK_LIBRARIES
        SWIFT_COMPILE_FLAGS
        SWIFT_COMPILE_FLAGS_IOS
        SWIFT_COMPILE_FLAGS_OSX
        SWIFT_COMPILE_FLAGS_TVOS
        SWIFT_COMPILE_FLAGS_WATCHOS
        SWIFT_COMPILE_FLAGS_LINUX
        SWIFT_MODULE_DEPENDS
        SWIFT_MODULE_DEPENDS_CYGWIN
        SWIFT_MODULE_DEPENDS_FREEBSD
        SWIFT_MODULE_DEPENDS_HAIKU
        SWIFT_MODULE_DEPENDS_IOS
        SWIFT_MODULE_DEPENDS_LINUX
        SWIFT_MODULE_DEPENDS_OSX
        SWIFT_MODULE_DEPENDS_TVOS
        SWIFT_MODULE_DEPENDS_WATCHOS
        SWIFT_MODULE_DEPENDS_WINDOWS
        SWIFT_MODULE_DEPENDS_FROM_SDK
        TARGET_SDKS
        SWIFT_COMPILE_FLAGS_MACCATALYST
        SWIFT_MODULE_DEPENDS_MACCATALYST
        SWIFT_MODULE_DEPENDS_MACCATALYST_UNZIPPERED)

  cmake_parse_arguments(SWIFTLIB
                        "${SWIFTLIB_options}"
                        "${SWIFTLIB_single_parameter_options}"
                        "${SWIFTLIB_multiple_parameter_options}"
                        ${ARGN})
  set(SWIFTLIB_SOURCES ${SWIFTLIB_UNPARSED_ARGUMENTS})

  # Ensure it's impossible to build for macCatalyst without module dependencies
  if(SWIFT_ENABLE_MACCATALYST AND SWIFTLIB_MACCATALYST_BUILD_FLAVOR)
    if((NOT SWIFTLIB_MACCATALYST_BUILD_FLAVOR STREQUAL "zippered") OR
       SWIFTLIB_SWIFT_MODULE_DEPENDS_OSX)
      precondition(SWIFTLIB_SWIFT_MODULE_DEPENDS_MACCATALYST
        MESSAGE "SWIFT_MODULE_DEPENDS_MACCATALYST is required when building for macCatalyst")
    endif()
  endif()

  # Infer arguments.

  if(SWIFTLIB_IS_SDK_OVERLAY)
    set(SWIFTLIB_HAS_SWIFT_CONTENT TRUE)
    set(SWIFTLIB_IS_STDLIB TRUE)
  endif()

  # Standard library is always a target library.
  if(SWIFTLIB_IS_STDLIB)
    set(SWIFTLIB_HAS_SWIFT_CONTENT TRUE)
  endif()

  # If target SDKs are not specified, build for all known SDKs.
  if("${SWIFTLIB_TARGET_SDKS}" STREQUAL "")
    set(SWIFTLIB_TARGET_SDKS ${SWIFT_SDKS})
  endif()
  list_replace(SWIFTLIB_TARGET_SDKS ALL_APPLE_PLATFORMS "${SWIFT_APPLE_PLATFORMS}")

  # All Swift code depends on the standard library, except for the standard
  # library itself.
  if(SWIFTLIB_HAS_SWIFT_CONTENT AND NOT SWIFTLIB_IS_STDLIB_CORE)
    list(APPEND SWIFTLIB_SWIFT_MODULE_DEPENDS Core)

    # swiftSwiftOnoneSupport does not depend on itself, obviously.
    if(NOT ${name} STREQUAL swiftSwiftOnoneSupport)
      # All Swift code depends on the SwiftOnoneSupport in non-optimized mode,
      # except for the standard library itself.
      is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" optimized)
      if(NOT optimized)
        list(APPEND SWIFTLIB_SWIFT_MODULE_DEPENDS SwiftOnoneSupport)
      endif()
    endif()
  endif()

  if((NOT "${SWIFT_BUILD_STDLIB}") AND
     (NOT "${SWIFTLIB_SWIFT_MODULE_DEPENDS}" STREQUAL ""))
    list(REMOVE_ITEM SWIFTLIB_SWIFT_MODULE_DEPENDS Core SwiftOnoneSupport)
  endif()

  translate_flags(SWIFTLIB "${SWIFTLIB_options}")
  precondition(SWIFTLIB_INSTALL_IN_COMPONENT MESSAGE "INSTALL_IN_COMPONENT is required")

  if(NOT SWIFTLIB_SHARED AND
     NOT SWIFTLIB_STATIC AND
     NOT SWIFTLIB_OBJECT_LIBRARY)
    message(FATAL_ERROR
        "Either SHARED, STATIC, or OBJECT_LIBRARY must be specified")
  endif()

  # In the standard library and overlays, warn about implicit overrides
  # as a reminder to consider when inherited protocols need different
  # behavior for their requirements.
  if (SWIFTLIB_IS_STDLIB)
    list(APPEND SWIFTLIB_SWIFT_COMPILE_FLAGS "-warn-implicit-overrides")
  endif()

  if(NOT SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER AND NOT BUILD_STANDALONE)
    list(APPEND SWIFTLIB_DEPENDS clang)
  endif()

  # If we are building this library for targets, loop through the various
  # SDKs building the variants of this library.
  list_intersect(
      "${SWIFTLIB_TARGET_SDKS}" "${SWIFT_SDKS}" SWIFTLIB_TARGET_SDKS)

  foreach(sdk ${SWIFTLIB_TARGET_SDKS})
    if(NOT SWIFT_SDK_${sdk}_ARCHITECTURES)
      # SWIFT_SDK_${sdk}_ARCHITECTURES is empty, so just continue
      continue()
    endif()

    # Skip building library for macOS if macCatalyst support is not enabled and the
    # library only builds for macOS when macCatalyst is enabled.
    if(NOT SWIFT_ENABLE_MACCATALYST AND
        sdk STREQUAL "OSX" AND
        SWIFTLIB_MACCATALYST_BUILD_FLAVOR STREQUAL "ios-like")
      message(STATUS "Skipping OSX SDK for module ${name}")
      continue()
    endif()

    # Determine if/what macCatalyst build flavor we are
    get_maccatalyst_build_flavor(maccatalyst_build_flavor
      "${sdk}" "${SWIFTLIB_MACCATALYST_BUILD_FLAVOR}")

    set(maccatalyst_build_flavors)
    if(NOT DEFINED maccatalyst_build_flavor)
       list(APPEND maccatalyst_build_flavors "none")
    elseif(maccatalyst_build_flavor STREQUAL "unzippered-twin")
      list(APPEND maccatalyst_build_flavors "macos-like" "ios-like")
    else()
      list(APPEND maccatalyst_build_flavors "${maccatalyst_build_flavor}")
    endif()

    # Loop over the build flavors for the this library. If it is an unzippered
    # twin we'll build it twice: once for "macos-like" and once for "ios-like"
    # flavors.
    foreach(maccatalyst_build_flavor ${maccatalyst_build_flavors})
    if(maccatalyst_build_flavor STREQUAL "none")
      unset(maccatalyst_build_flavor)
    endif()

    set(THIN_INPUT_TARGETS)

    # Collect architecture agnostic SDK module dependencies
    set(swiftlib_module_depends_flattened ${SWIFTLIB_SWIFT_MODULE_DEPENDS})
    if(${sdk} STREQUAL OSX)
       if(DEFINED maccatalyst_build_flavor AND NOT maccatalyst_build_flavor STREQUAL "macos-like")
          list(APPEND swiftlib_module_depends_flattened
            ${SWIFTLIB_SWIFT_MODULE_DEPENDS_MACCATALYST})
          list(APPEND swiftlib_module_depends_flattened
            ${SWIFTLIB_SWIFT_MODULE_DEPENDS_MACCATALYST_UNZIPPERED})
        else()
          list(APPEND swiftlib_module_depends_flattened
            ${SWIFTLIB_SWIFT_MODULE_DEPENDS_OSX})
        endif()
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_OSX})
    elseif(${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_IOS})
    elseif(${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_TVOS})
    elseif(${sdk} STREQUAL WATCHOS OR ${sdk} STREQUAL WATCHOS_SIMULATOR)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_WATCHOS})
    elseif(${sdk} STREQUAL FREEBSD)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_FREEBSD})
    elseif(${sdk} STREQUAL LINUX OR ${sdk} STREQUAL ANDROID)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_LINUX})
    elseif(${sdk} STREQUAL CYGWIN)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_CYGWIN})
    elseif(${sdk} STREQUAL HAIKU)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_HAIKU})
    elseif(${sdk} STREQUAL WINDOWS)
      list(APPEND swiftlib_module_depends_flattened
           ${SWIFTLIB_SWIFT_MODULE_DEPENDS_WINDOWS})
    endif()

    # Collect architecture agnostic SDK framework dependencies
    set(swiftlib_framework_depends_flattened ${SWIFTLIB_FRAMEWORK_DEPENDS})
    if(${sdk} STREQUAL OSX)
      list(APPEND swiftlib_framework_depends_flattened
           ${SWIFTLIB_FRAMEWORK_DEPENDS_OSX})
    elseif(${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR OR
           ${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR)
      list(APPEND swiftlib_framework_depends_flattened
           ${SWIFTLIB_FRAMEWORK_DEPENDS_IOS_TVOS})
    endif()

    # Collect architecutre agnostic compiler flags
    set(swiftlib_swift_compile_flags_all ${SWIFTLIB_SWIFT_COMPILE_FLAGS})
    if(${sdk} STREQUAL OSX)
      list(APPEND swiftlib_swift_compile_flags_all
           ${SWIFTLIB_SWIFT_COMPILE_FLAGS_OSX})
    elseif(${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR)
      list(APPEND swiftlib_swift_compile_flags_all
           ${SWIFTLIB_SWIFT_COMPILE_FLAGS_IOS})
    elseif(${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR)
      list(APPEND swiftlib_swift_compile_flags_all
           ${SWIFTLIB_SWIFT_COMPILE_FLAGS_TVOS})
    elseif(${sdk} STREQUAL WATCHOS OR ${sdk} STREQUAL WATCHOS_SIMULATOR)
      list(APPEND swiftlib_swift_compile_flags_all
           ${SWIFTLIB_SWIFT_COMPILE_FLAGS_WATCHOS})
    elseif(${sdk} STREQUAL LINUX)
      list(APPEND swiftlib_swift_compile_flags_all
           ${SWIFTLIB_SWIFT_COMPILE_FLAGS_LINUX})
    elseif(${sdk} STREQUAL WINDOWS)
      # FIXME(SR2005) static and shared are not mutually exclusive; however
      # since we do a single build of the sources, this doesn't work for
      # building both simultaneously.  Effectively, only shared builds are
      # supported on windows currently.
      if(SWIFTLIB_SHARED)
        list(APPEND swiftlib_swift_compile_flags_all -D_WINDLL)
        if(SWIFTLIB_IS_STDLIB_CORE)
          list(APPEND swiftlib_swift_compile_flags_all -DswiftCore_EXPORTS)
        endif()
      elseif(SWIFTLIB_STATIC)
        list(APPEND swiftlib_swift_compile_flags_all -D_LIB)
      endif()
    endif()

    # SWIFT_ENABLE_TENSORFLOW
    # NOTE(TF-1021): Enable cross-file derivative registration for stdlib.
    list(APPEND swiftlib_swift_compile_flags_all
         -Xfrontend -enable-experimental-cross-file-derivative-registration)
    # SWIFT_ENABLE_TENSORFLOW END

    # Collect architecture agnostic SDK linker flags
    set(swiftlib_link_flags_all ${SWIFTLIB_LINK_FLAGS})
    if(${sdk} STREQUAL IOS_SIMULATOR AND ${name} STREQUAL swiftMediaPlayer)
      # message("DISABLING AUTOLINK FOR swiftMediaPlayer")
      list(APPEND swiftlib_link_flags_all "-Xlinker" "-ignore_auto_link")
    endif()

    # We unconditionally removed "-z,defs" from CMAKE_SHARED_LINKER_FLAGS in
    # swift_common_standalone_build_config_llvm within
    # SwiftSharedCMakeConfig.cmake, where it was added by a call to
    # HandleLLVMOptions.
    #
    # Rather than applying it to all targets and libraries, we here add it
    # back to supported targets and libraries only.  This is needed for ELF
    # targets only; however, RemoteMirror needs to build with undefined
    # symbols.
    if(${SWIFT_SDK_${sdk}_OBJECT_FORMAT} STREQUAL ELF AND
       NOT ${name} STREQUAL swiftRemoteMirror)
      list(APPEND swiftlib_link_flags_all "-Wl,-z,defs")
    endif()
    # Setting back linker flags which are not supported when making Android build on macOS cross-compile host.
    if(SWIFTLIB_SHARED)
      if(sdk IN_LIST SWIFT_APPLE_PLATFORMS)
        list(APPEND swiftlib_link_flags_all "-dynamiclib -Wl,-headerpad_max_install_names")
      elseif(${sdk} STREQUAL ANDROID)
        list(APPEND swiftlib_link_flags_all "-shared")
        # TODO: Instead of `lib${name}.so` find variable or target property which already have this value.
        list(APPEND swiftlib_link_flags_all "-Wl,-soname,lib${name}.so")
      endif()
    endif()

    set(sdk_supported_archs
      ${SWIFT_SDK_${sdk}_ARCHITECTURES}
      ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
    list(REMOVE_DUPLICATES sdk_supported_archs)

    # For each architecture supported by this SDK
    foreach(arch ${sdk_supported_archs})
      # Configure variables for this subdirectory.
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")
      set(MODULE_VARIANT_SUFFIX "-swiftmodule${VARIANT_SUFFIX}")
      set(MODULE_VARIANT_NAME "${name}${MODULE_VARIANT_SUFFIX}")

      # Configure macCatalyst flavor variables
      if(DEFINED maccatalyst_build_flavor)
        set(maccatalyst_variant_suffix "-${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}-${arch}")
        set(maccatalyst_variant_name "${name}${maccatalyst_variant_suffix}")

        set(maccatalyst_module_variant_suffix "-swiftmodule${maccatalyst_variant_suffix}")
        set(maccatalyst_module_variant_name "${name}${maccatalyst_module_variant_suffix}")
      endif()

      # Map dependencies over to the appropriate variants.
      set(swiftlib_link_libraries)
      foreach(lib ${SWIFTLIB_LINK_LIBRARIES})
        if(TARGET "${lib}${VARIANT_SUFFIX}")
          list(APPEND swiftlib_link_libraries "${lib}${VARIANT_SUFFIX}")
        else()
          list(APPEND swiftlib_link_libraries "${lib}")
        endif()
      endforeach()

      # Swift compiles depend on swift modules, while links depend on
      # linked libraries.  Find targets for both of these here.
      set(swiftlib_module_dependency_targets)
      set(swiftlib_private_link_libraries_targets)

      if(NOT BUILD_STANDALONE)
        foreach(mod ${swiftlib_module_depends_flattened})
          if(DEFINED maccatalyst_build_flavor)
            if(maccatalyst_build_flavor STREQUAL "zippered")
              # Zippered libraries are dependent on both the macCatalyst and normal macOS
              # modules of their dependencies (which themselves must be zippered).
              list(APPEND swiftlib_module_dependency_targets
                   "swift${mod}${maccatalyst_module_variant_suffix}")
              list(APPEND swiftlib_module_dependency_targets
                   "swift${mod}${MODULE_VARIANT_SUFFIX}")

              # Zippered libraries link against their zippered library targets, which
              # live (and are built in) the same location as normal macOS libraries.
              list(APPEND swiftlib_private_link_libraries_targets
                "swift${mod}${VARIANT_SUFFIX}")
            elseif(maccatalyst_build_flavor STREQUAL "ios-like")
              # iOS-like libraries depend on the macCatalyst modules of their dependencies
              # regardless of whether the target is zippered or macCatalyst only.
              list(APPEND swiftlib_module_dependency_targets
                   "swift${mod}${maccatalyst_module_variant_suffix}")

              # iOS-like libraries can link against either iOS-like library targets
              # or zippered targets.
              if(mod IN_LIST SWIFTLIB_SWIFT_MODULE_DEPENDS_MACCATALYST_UNZIPPERED)
                list(APPEND swiftlib_private_link_libraries_targets
                    "swift${mod}${maccatalyst_variant_suffix}")
              else()
                list(APPEND swiftlib_private_link_libraries_targets
                    "swift${mod}${VARIANT_SUFFIX}")
              endif()
            else()
              list(APPEND swiftlib_module_dependency_targets
                   "swift${mod}${MODULE_VARIANT_SUFFIX}")

              list(APPEND swiftlib_private_link_libraries_targets
                 "swift${mod}${VARIANT_SUFFIX}")
            endif()
            continue()
          endif()

          list(APPEND swiftlib_module_dependency_targets
              "swift${mod}${MODULE_VARIANT_SUFFIX}")

          list(APPEND swiftlib_private_link_libraries_targets
              "swift${mod}${VARIANT_SUFFIX}")
        endforeach()
      endif()

      foreach(lib ${SWIFTLIB_PRIVATE_LINK_LIBRARIES})
        if(TARGET "${lib}${VARIANT_SUFFIX}")
          list(APPEND swiftlib_private_link_libraries_targets
              "${lib}${VARIANT_SUFFIX}")
        else()
          list(APPEND swiftlib_private_link_libraries_targets "${lib}")
        endif()
      endforeach()

      # Add PrivateFrameworks, rdar://28466433
      set(swiftlib_c_compile_flags_all ${SWIFTLIB_C_COMPILE_FLAGS})
      set(swiftlib_link_flags_all ${SWIFTLIB_LINK_FLAGS})

      # Add flags to prepend framework search paths for the parallel framework
      # hierarchy rooted at /System/iOSSupport/...
      # These paths must come before their normal counterparts so that when compiling
      # macCatalyst-only or unzippered-twin overlays the macCatalyst version
      # of a framework is found and not the Mac version.
      if(maccatalyst_build_flavor STREQUAL "ios-like"
          OR (name STREQUAL "swiftXCTest"
            AND maccatalyst_build_flavor STREQUAL "zippered"))

        # The path to find iOS-only frameworks (such as UIKit) under macCatalyst.
        set(ios_support_frameworks_path "${SWIFT_SDK_${sdk}_PATH}/System/iOSSupport/System/Library/Frameworks/")

        list(APPEND swiftlib_swift_compile_flags_all "-Fsystem" "${ios_support_frameworks_path}")
        list(APPEND swiftlib_c_compile_flags_all "-iframework" "${ios_support_frameworks_path}")
        # We collate -F with the framework path to avoid unwanted deduplication
        # of options by target_compile_options -- this way no undesired
        # side effects are introduced should a new search path be added.
        list(APPEND swiftlib_link_flags_all "-F${ios_support_frameworks_path}")
      endif()

      if(sdk IN_LIST SWIFT_APPLE_PLATFORMS AND SWIFTLIB_IS_SDK_OVERLAY)
        set(swiftlib_swift_compile_private_frameworks_flag "-Fsystem" "${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}/System/Library/PrivateFrameworks/")
        foreach(tbd_lib ${SWIFTLIB_SWIFT_MODULE_DEPENDS_FROM_SDK})
          list(APPEND swiftlib_link_flags_all "${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}/usr/lib/swift/libswift${tbd_lib}.tbd")
        endforeach()
      endif()

      set(variant_name "${VARIANT_NAME}")
      set(module_variant_names "${MODULE_VARIANT_NAME}")
      if(maccatalyst_build_flavor STREQUAL "ios-like")
        set(variant_name "${maccatalyst_variant_name}")
        set(module_variant_names "${maccatalyst_module_variant_name}")
      elseif(maccatalyst_build_flavor STREQUAL "zippered")
        # Zippered libraries produce two modules: one for macCatalyst and one for macOS
        # and so need two module targets.
        list(APPEND module_variant_names "${maccatalyst_module_variant_name}")
      endif()

     list(APPEND swiftlib_c_compile_flags_all "-DSWIFT_TARGET_LIBRARY_NAME=${name}")

      # Add this library variant.
      _add_swift_target_library_single(
        ${variant_name}
        ${name}
        ${SWIFTLIB_SHARED_keyword}
        ${SWIFTLIB_STATIC_keyword}
        ${SWIFTLIB_OBJECT_LIBRARY_keyword}
        ${SWIFTLIB_INSTALL_WITH_SHARED_keyword}
        ${SWIFTLIB_SOURCES}
        TARGET_LIBRARY
        MODULE_TARGETS ${module_variant_names}
        SDK ${sdk}
        ARCHITECTURE ${arch}
        DEPENDS ${SWIFTLIB_DEPENDS}
        LINK_LIBRARIES ${swiftlib_link_libraries}
        FRAMEWORK_DEPENDS ${swiftlib_framework_depends_flattened}
        FRAMEWORK_DEPENDS_WEAK ${SWIFTLIB_FRAMEWORK_DEPENDS_WEAK}
        LLVM_LINK_COMPONENTS ${SWIFTLIB_LLVM_LINK_COMPONENTS}
        FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS} ${swiftlib_module_dependency_targets}
        C_COMPILE_FLAGS ${swiftlib_c_compile_flags_all}
        SWIFT_COMPILE_FLAGS ${swiftlib_swift_compile_flags_all} ${swiftlib_swift_compile_flags_arch} ${swiftlib_swift_compile_private_frameworks_flag}
        LINK_FLAGS ${swiftlib_link_flags_all}
        PRIVATE_LINK_LIBRARIES ${swiftlib_private_link_libraries_targets}
        INCORPORATE_OBJECT_LIBRARIES ${SWIFTLIB_INCORPORATE_OBJECT_LIBRARIES}
        INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY ${SWIFTLIB_INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY}
        ${SWIFTLIB_DONT_EMBED_BITCODE_keyword}
        ${SWIFTLIB_IS_STDLIB_keyword}
        ${SWIFTLIB_IS_STDLIB_CORE_keyword}
        ${SWIFTLIB_IS_SDK_OVERLAY_keyword}
        ${SWIFTLIB_NOSWIFTRT_keyword}
        DARWIN_INSTALL_NAME_DIR "${SWIFTLIB_DARWIN_INSTALL_NAME_DIR}"
        INSTALL_IN_COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
        DEPLOYMENT_VERSION_OSX "${SWIFTLIB_DEPLOYMENT_VERSION_OSX}"
        DEPLOYMENT_VERSION_MACCATALYST "${SWIFTLIB_DEPLOYMENT_VERSION_MACCATALYST}"
        DEPLOYMENT_VERSION_IOS "${SWIFTLIB_DEPLOYMENT_VERSION_IOS}"
        DEPLOYMENT_VERSION_TVOS "${SWIFTLIB_DEPLOYMENT_VERSION_TVOS}"
        DEPLOYMENT_VERSION_WATCHOS "${SWIFTLIB_DEPLOYMENT_VERSION_WATCHOS}"
        MACCATALYST_BUILD_FLAVOR "${maccatalyst_build_flavor}"

        GYB_SOURCES ${SWIFTLIB_GYB_SOURCES}
      )
    if(NOT SWIFT_BUILT_STANDALONE AND NOT "${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
      add_dependencies(${VARIANT_NAME} clang)
    endif()

      if(sdk STREQUAL WINDOWS)
        if(SWIFT_COMPILER_IS_MSVC_LIKE)
          if (SWIFT_STDLIB_MSVC_RUNTIME_LIBRARY MATCHES MultiThreadedDebugDLL)
            target_compile_options(${VARIANT_NAME} PRIVATE /MDd /D_DLL /D_DEBUG)
          elseif (SWIFT_STDLIB_MSVC_RUNTIME_LIBRARY MATCHES MultiThreadedDebug)
            target_compile_options(${VARIANT_NAME} PRIVATE /MTd /U_DLL /D_DEBUG)
          elseif (SWIFT_STDLIB_MSVC_RUNTIME_LIBRARY MATCHES MultiThreadedDLL)
            target_compile_options(${VARIANT_NAME} PRIVATE /MD /D_DLL /U_DEBUG)
          elseif (SWIFT_STDLIB_MSVC_RUNTIME_LIBRARY MATCHES MultiThreaded)
            target_compile_options(${VARIANT_NAME} PRIVATE /MT /U_DLL /U_DEBUG)
          endif()
        endif()
      endif()

      if(NOT SWIFTLIB_OBJECT_LIBRARY)
        # Add dependencies on the (not-yet-created) custom lipo target.
        foreach(DEP ${SWIFTLIB_LINK_LIBRARIES})
          if (NOT "${DEP}" STREQUAL "icucore")
            add_dependencies(${VARIANT_NAME}
              "${DEP}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
          endif()
        endforeach()

        if (SWIFTLIB_IS_STDLIB AND SWIFTLIB_STATIC)
          # Add dependencies on the (not-yet-created) custom lipo target.
          foreach(DEP ${SWIFTLIB_LINK_LIBRARIES})
            if (NOT "${DEP}" STREQUAL "icucore")
              add_dependencies("${VARIANT_NAME}-static"
                "${DEP}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-static")
            endif()
          endforeach()
        endif()

        if(arch IN_LIST SWIFT_SDK_${sdk}_ARCHITECTURES)
          # Note this thin library.
          list(APPEND THIN_INPUT_TARGETS ${VARIANT_NAME})
        endif()
      endif()
    endforeach()

    # Configure module-only targets
    if(NOT SWIFT_SDK_${sdk}_ARCHITECTURES
        AND SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES)
      set(_target "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}")

      # Create unified sdk target
      add_custom_target("${_target}")

      foreach(_arch ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
        set(_variant_suffix "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${_arch}")
        set(_module_variant_name "${name}-swiftmodule-${_variant_suffix}")

        add_dependencies("${_target}" ${_module_variant_name})

        # Add Swift standard library targets as dependencies to the top-level
        # convenience target.
        if(TARGET "swift-stdlib${_variant_suffix}")
          add_dependencies("swift-stdlib${_variant_suffix}"
            "${_target}")
        endif()
      endforeach()

      return()
    endif()

    set(library_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
    if(maccatalyst_build_flavor STREQUAL "ios-like")
      set(library_subdir "${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}")
    endif()

    if(NOT SWIFTLIB_OBJECT_LIBRARY)
      # Determine the name of the universal library.
      if(SWIFTLIB_SHARED)
        if("${sdk}" STREQUAL "WINDOWS")
          set(UNIVERSAL_LIBRARY_NAME
            "${SWIFTLIB_DIR}/${library_subdir}/${name}.dll")
        else()
          set(UNIVERSAL_LIBRARY_NAME
            "${SWIFTLIB_DIR}/${library_subdir}/${CMAKE_SHARED_LIBRARY_PREFIX}${name}${CMAKE_SHARED_LIBRARY_SUFFIX}")
        endif()
      else()
        if("${sdk}" STREQUAL "WINDOWS")
          set(UNIVERSAL_LIBRARY_NAME
            "${SWIFTLIB_DIR}/${library_subdir}/${name}.lib")
        else()
          set(UNIVERSAL_LIBRARY_NAME
            "${SWIFTLIB_DIR}/${library_subdir}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}")
        endif()
      endif()

      set(lipo_target "${name}-${library_subdir}")
      if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin" AND SWIFTLIB_SHARED)
        set(codesign_arg CODESIGN)
      endif()
      precondition(THIN_INPUT_TARGETS)
      _add_swift_lipo_target(SDK
                               ${sdk}
                             TARGET
                               ${lipo_target}
                             OUTPUT
                               ${UNIVERSAL_LIBRARY_NAME}
                             ${codesign_arg}
                             ${THIN_INPUT_TARGETS})

      # Cache universal libraries for dependency purposes
      set(UNIVERSAL_LIBRARY_NAMES_${library_subdir}
        ${UNIVERSAL_LIBRARY_NAMES_${library_subdir}}
        ${lipo_target}
        CACHE INTERNAL "UNIVERSAL_LIBRARY_NAMES_${library_subdir}")

      # Determine the subdirectory where this library will be installed.
      set(resource_dir_sdk_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
      if(maccatalyst_build_flavor STREQUAL "ios-like")
        set(resource_dir_sdk_subdir "${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}")
      endif()

      precondition(resource_dir_sdk_subdir)

      if(SWIFTLIB_SHARED OR SWIFTLIB_INSTALL_WITH_SHARED)
        set(resource_dir "swift")
        set(file_permissions
            OWNER_READ OWNER_WRITE OWNER_EXECUTE
            GROUP_READ GROUP_EXECUTE
            WORLD_READ WORLD_EXECUTE)
      else()
        set(resource_dir "swift_static")
        set(file_permissions
            OWNER_READ OWNER_WRITE
            GROUP_READ
            WORLD_READ)
      endif()

      set(optional_arg)
      if(sdk IN_LIST SWIFT_APPLE_PLATFORMS)
        # Allow installation of stdlib without building all variants on Darwin.
        set(optional_arg "OPTIONAL")
      endif()

      if(sdk STREQUAL WINDOWS AND CMAKE_SYSTEM_NAME STREQUAL Windows)
        add_dependencies(${SWIFTLIB_INSTALL_IN_COMPONENT} ${name}-windows-${SWIFT_PRIMARY_VARIANT_ARCH})
        swift_install_in_component(TARGETS ${name}-windows-${SWIFT_PRIMARY_VARIANT_ARCH}
                                   RUNTIME
                                     DESTINATION "bin"
                                     COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
                                   LIBRARY
                                     DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/${resource_dir}/${resource_dir_sdk_subdir}/${SWIFT_PRIMARY_VARIANT_ARCH}"
                                     COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
                                   ARCHIVE
                                     DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/${resource_dir}/${resource_dir_sdk_subdir}/${SWIFT_PRIMARY_VARIANT_ARCH}"
                                     COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
                                   PERMISSIONS ${file_permissions})
      else()
        # NOTE: ${UNIVERSAL_LIBRARY_NAME} is the output associated with the target
        # ${lipo_target}
        add_dependencies(${SWIFTLIB_INSTALL_IN_COMPONENT} ${lipo_target})
        swift_install_in_component(FILES "${UNIVERSAL_LIBRARY_NAME}"
                                   DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/${resource_dir}/${resource_dir_sdk_subdir}"
                                   COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
                                   PERMISSIONS ${file_permissions}
                                   "${optional_arg}")
      endif()
      if(sdk STREQUAL WINDOWS)
        foreach(arch ${SWIFT_SDK_WINDOWS_ARCHITECTURES})
          if(TARGET ${name}-windows-${arch}_IMPLIB)
            get_target_property(import_library ${name}-windows-${arch}_IMPLIB IMPORTED_LOCATION)
            add_dependencies(${SWIFTLIB_INSTALL_IN_COMPONENT} ${name}-windows-${arch}_IMPLIB)
            swift_install_in_component(FILES ${import_library}
                                       DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/${resource_dir}/${resource_dir_sdk_subdir}/${arch}"
                                       COMPONENT ${SWIFTLIB_INSTALL_IN_COMPONENT}
                                       PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
          endif()
        endforeach()
      endif()

      swift_is_installing_component(
        "${SWIFTLIB_INSTALL_IN_COMPONENT}"
        is_installing)

      # Add the arch-specific library targets to the global exports.
      foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
        set(_variant_name "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
        if(maccatalyst_build_flavor STREQUAL "ios-like")
          set(_variant_name "${name}-${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}-${arch}")
        endif()

        if(NOT TARGET "${_variant_name}")
          continue()
        endif()

        if(is_installing)
          set_property(GLOBAL APPEND
            PROPERTY SWIFT_EXPORTS ${_variant_name})
        else()
          set_property(GLOBAL APPEND
            PROPERTY SWIFT_BUILDTREE_EXPORTS ${_variant_name})
        endif()
      endforeach()

      # Add the swiftmodule-only targets to the lipo target depdencies.
      foreach(arch ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
        set(_variant_name "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
        if(maccatalyst_build_flavor STREQUAL "ios-like")
          set(_variant_name "${name}-${SWIFT_SDK_MACCATALYST_LIB_SUBDIR}-${arch}")
        endif()

        if(NOT TARGET "${_variant_name}")
          continue()
        endif()

        add_dependencies("${lipo_target}" "${_variant_name}")
      endforeach()

      # If we built static variants of the library, create a lipo target for
      # them.
      set(lipo_target_static)
      if (SWIFTLIB_IS_STDLIB AND SWIFTLIB_STATIC)
        set(THIN_INPUT_TARGETS_STATIC)
        foreach(TARGET ${THIN_INPUT_TARGETS})
          list(APPEND THIN_INPUT_TARGETS_STATIC "${TARGET}-static")
        endforeach()

        if(SWIFTLIB_INSTALL_WITH_SHARED)
          set(install_subdir "swift")
          set(universal_subdir ${SWIFTLIB_DIR})
        else()
          set(install_subdir "swift_static")
          set(universal_subdir ${SWIFTSTATICLIB_DIR})
        endif()

        set(lipo_target_static
            "${name}-${library_subdir}-static")
        set(UNIVERSAL_LIBRARY_NAME
            "${universal_subdir}/${library_subdir}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}")
        _add_swift_lipo_target(SDK
                                 ${sdk}
                               TARGET
                                 ${lipo_target_static}
                               OUTPUT
                                 "${UNIVERSAL_LIBRARY_NAME}"
                               ${THIN_INPUT_TARGETS_STATIC})
        add_dependencies(${SWIFTLIB_INSTALL_IN_COMPONENT} ${lipo_target_static})
        swift_install_in_component(FILES "${UNIVERSAL_LIBRARY_NAME}"
                                   DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/${install_subdir}/${resource_dir_sdk_subdir}"
                                   PERMISSIONS
                                     OWNER_READ OWNER_WRITE
                                     GROUP_READ
                                     WORLD_READ
                                   COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}"
                                   "${optional_arg}")
      endif()

      # Add Swift standard library targets as dependencies to the top-level
      # convenience target.
      set(FILTERED_UNITTESTS
            swiftStdlibCollectionUnittest
            swiftStdlibUnicodeUnittest)

      foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
        set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
        if(TARGET "swift-stdlib${VARIANT_SUFFIX}" AND
           TARGET "swift-test-stdlib${VARIANT_SUFFIX}")
          add_dependencies("swift-stdlib${VARIANT_SUFFIX}"
              ${lipo_target}
              ${lipo_target_static})
          if(NOT "${name}" IN_LIST FILTERED_UNITTESTS)
            add_dependencies("swift-test-stdlib${VARIANT_SUFFIX}"
                ${lipo_target}
                ${lipo_target_static})
          endif()
        endif()
      endforeach()
    endif()
  endforeach() # maccatalyst_build_flavors
  endforeach()
endfunction()

# Add an executable compiled for a given variant.
#
# Don't use directly, use add_swift_executable and add_swift_target_executable
# instead.
#
# See add_swift_executable for detailed documentation.
#
# Additional parameters:
#   [SDK sdk]
#     SDK to build for.
#
#   [ARCHITECTURE architecture]
#     Architecture to build for.
function(_add_swift_target_executable_single name)
  set(options)
  set(single_parameter_options
    ARCHITECTURE
    SDK)
  set(multiple_parameter_options
    COMPILE_FLAGS
    DEPENDS
    LLVM_LINK_COMPONENTS)
  cmake_parse_arguments(SWIFTEXE_SINGLE
    "${options}"
    "${single_parameter_options}"
    "${multiple_parameter_options}"
    ${ARGN})

  set(SWIFTEXE_SINGLE_SOURCES ${SWIFTEXE_SINGLE_UNPARSED_ARGUMENTS})

  # Check arguments.
  precondition(SWIFTEXE_SINGLE_SDK MESSAGE "Should specify an SDK")
  precondition(SWIFTEXE_SINGLE_ARCHITECTURE MESSAGE "Should specify an architecture")

  # Determine compiler flags.
  set(c_compile_flags)
  set(link_flags)

  # Prepare linker search directories.
  set(library_search_directories
        "${SWIFTLIB_DIR}/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")

  # Add variant-specific flags.
  _add_variant_c_compile_flags(
    SDK "${SWIFTEXE_SINGLE_SDK}"
    ARCH "${SWIFTEXE_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${CMAKE_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${LLVM_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    ANALYZE_CODE_COVERAGE "${SWIFT_ANALYZE_CODE_COVERAGE}"
    RESULT_VAR_NAME c_compile_flags)
  _add_variant_link_flags(
    SDK "${SWIFTEXE_SINGLE_SDK}"
    ARCH "${SWIFTEXE_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${CMAKE_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${LLVM_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    LTO_OBJECT_NAME "${name}-${SWIFTEXE_SINGLE_SDK}-${SWIFTEXE_SINGLE_ARCHITECTURE}"
    ANALYZE_CODE_COVERAGE "${SWIFT_ANALYZE_CODE_COVERAGE}"
    RESULT_VAR_NAME link_flags
    LINK_LIBRARIES_VAR_NAME link_libraries
    LIBRARY_SEARCH_DIRECTORIES_VAR_NAME library_search_directories)

  handle_swift_sources(
      dependency_target
      unused_module_dependency_target
      unused_sib_dependency_target
      unused_sibopt_dependency_target
      unused_sibgen_dependency_target
      SWIFTEXE_SINGLE_SOURCES SWIFTEXE_SINGLE_EXTERNAL_SOURCES ${name}
      DEPENDS
        ${SWIFTEXE_SINGLE_DEPENDS}
      MODULE_NAME ${name}
      SDK ${SWIFTEXE_SINGLE_SDK}
      ARCHITECTURE ${SWIFTEXE_SINGLE_ARCHITECTURE}
      COMPILE_FLAGS ${SWIFTEXE_SINGLE_COMPILE_FLAGS}
      IS_MAIN)
  add_swift_source_group("${SWIFTEXE_SINGLE_EXTERNAL_SOURCES}")

  add_executable(${name}
      ${SWIFTEXE_SINGLE_SOURCES}
      ${SWIFTEXE_SINGLE_EXTERNAL_SOURCES})

  add_dependencies_multiple_targets(
      TARGETS "${name}"
      DEPENDS
        ${dependency_target}
        ${LLVM_COMMON_DEPENDS}
        ${SWIFTEXE_SINGLE_DEPENDS})
  llvm_update_compile_flags("${name}")

  if(SWIFTEXE_SINGLE_SDK STREQUAL WINDOWS)
    swift_windows_include_for_arch(${SWIFTEXE_SINGLE_ARCHITECTURE}
      ${SWIFTEXE_SINGLE_ARCHITECTURE}_INCLUDE)
    target_include_directories(${name} SYSTEM PRIVATE
      ${${SWIFTEXE_SINGLE_ARCHITECTURE}_INCLUDE})

    if(NOT ${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
      # MSVC doesn't support -Xclang. We don't need to manually specify
      # the dependent libraries as `cl` does so.
      target_compile_options(${name} PRIVATE
        "SHELL:-Xclang --dependent-lib=oldnames"
        # TODO(compnerd) handle /MT, /MTd
        "SHELL:-Xclang --dependent-lib=msvcrt$<$<CONFIG:Debug>:d>")
    endif()
  endif()
  target_compile_options(${name} PRIVATE
    ${c_compile_flags})
  target_link_directories(${name} PRIVATE
    ${library_search_directories})
  target_link_options(${name} PRIVATE
    ${link_flags})
  target_link_libraries(${name} PRIVATE
    ${link_libraries})
  if (SWIFT_PARALLEL_LINK_JOBS)
    set_property(TARGET ${name} PROPERTY JOB_POOL_LINK swift_link_job_pool)
  endif()
  if(${SWIFTEXE_SINGLE_SDK} IN_LIST SWIFT_APPLE_PLATFORMS)
    set_target_properties(${name} PROPERTIES
      BUILD_WITH_INSTALL_RPATH YES
      INSTALL_RPATH "@executable_path/../lib/swift/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")
  endif()
  set_output_directory(${name}
      BINARY_DIR ${SWIFT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  swift_common_llvm_config("${name}" ${SWIFTEXE_SINGLE_LLVM_LINK_COMPONENTS})

  # NOTE(compnerd) use the C linker language to invoke `clang` rather than
  # `clang++` as we explicitly link against the C++ runtime.  We were previously
  # actually passing `-nostdlib++` to avoid the C++ runtime linkage.
  if(${SWIFTEXE_SINGLE_SDK} STREQUAL ANDROID)
    set_property(TARGET "${name}" PROPERTY
      LINKER_LANGUAGE "C")
  else()
    set_property(TARGET "${name}" PROPERTY
      LINKER_LANGUAGE "CXX")
  endif()

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")
endfunction()

# Add an executable for each target variant. Executables are given suffixes
# with the variant SDK and ARCH.
#
# See add_swift_executable for detailed documentation.
function(add_swift_target_executable name)
  # Parse the arguments we were given.
  cmake_parse_arguments(SWIFTEXE_TARGET
    "EXCLUDE_FROM_ALL;;BUILD_WITH_STDLIB"
    ""
    "DEPENDS;LLVM_LINK_COMPONENTS;LINK_LIBRARIES"
    ${ARGN})

  set(SWIFTEXE_TARGET_SOURCES ${SWIFTEXE_TARGET_UNPARSED_ARGUMENTS})

  if(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL)
    message(SEND_ERROR "${name} is using EXCLUDE_FROM_ALL which is deprecated.")
  endif()

  # All Swift executables depend on the standard library.
  list(APPEND SWIFTEXE_TARGET_LINK_LIBRARIES swiftCore)
  # All Swift executables depend on the swiftSwiftOnoneSupport library.
  list(APPEND SWIFTEXE_TARGET_DEPENDS swiftSwiftOnoneSupport)

  foreach(sdk ${SWIFT_SDKS})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

      if(SWIFTEXE_TARGET_BUILD_WITH_STDLIB)
        add_dependencies("swift-test-stdlib${VARIANT_SUFFIX}" ${VARIANT_NAME})
      endif()

      # Don't add the ${arch} to the suffix.  We want to link against fat
      # libraries.
      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_DEPENDS}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}"
          SWIFTEXE_TARGET_DEPENDS_with_suffix)
      _add_swift_target_executable_single(
          ${VARIANT_NAME}
          ${SWIFTEXE_TARGET_SOURCES}
          DEPENDS ${SWIFTEXE_TARGET_DEPENDS_with_suffix}
          LLVM_LINK_COMPONENTS ${SWIFTEXE_TARGET_LLVM_LINK_COMPONENTS}
          SDK "${sdk}"
          ARCHITECTURE "${arch}")

      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_LINK_LIBRARIES}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}"
          SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS)
      target_link_libraries(${VARIANT_NAME} PRIVATE
          ${SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS})

      if(NOT "${VARIANT_SUFFIX}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SUFFIX}")
        # By default, don't build executables for target SDKs to avoid building
        # target stdlibs.
        set_target_properties(${VARIANT_NAME} PROPERTIES
          EXCLUDE_FROM_ALL TRUE)
      endif()

      if(${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
        add_custom_command_target(unused_var2
         COMMAND "codesign" "-f" "-s" "-" "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}"
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
      else()
        # No code signing on other platforms.
        add_custom_command_target(unused_var2
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
       endif()
    endforeach()
  endforeach()
endfunction()
