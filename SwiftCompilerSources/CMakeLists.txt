# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

# Following function are needed as a workaround until it's possible to compile
# swift code with cmake's builtin swift support.

# Add a swift compiler module
#
# Creates a target to compile a swift module.
# Adds the module name to the global property "swift_compiler_modules".
#
function(add_swift_compiler_module module)
  cmake_parse_arguments(ALSM
                        "ADD_TO_SYNTAXPARSE"
                        ""
                        "DEPENDS;SOURCES"
                        ${ARGN})
  set(raw_sources ${ALSM_SOURCES} ${ALSM_UNPARSED_ARGUMENTS})

  set(target_name "SwiftModule${module}")

  # Add a target which depends on the actual compilation target, which
  # will be created in add_swift_compiler_modules_library.
  # This target is mainly used to add properties, like the list of source files.
  add_custom_target(
      ${target_name}
      COMMENT "swift compiler module ${module}")

  swift_compiler_sources(${module} ${raw_sources})

  set_property(TARGET ${target_name} PROPERTY module_name ${module})
  set_property(TARGET ${target_name} PROPERTY module_depends ${ALSM_DEPENDS})
  set_property(TARGET ${target_name} PROPERTY add_to_syntaxparse ${ALSM_ADD_TO_SYNTAXPARSE})

  get_property(modules GLOBAL PROPERTY swift_compiler_modules)
  set_property(GLOBAL PROPERTY swift_compiler_modules ${modules} ${module})
endfunction()
 
# Add source files to a swift compiler module.
#
function(swift_compiler_sources module)
  cmake_parse_arguments(LSS
                        ""
                        ""
                        ""
                        ${ARGN})
  set(raw_sources ${LSS_UNPARSED_ARGUMENTS})
  set(sources)
  foreach(raw_source ${raw_sources})
    get_filename_component(
      raw_source "${raw_source}" REALPATH BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
    list(APPEND sources "${raw_source}")
  endforeach()

  set(target_name "SwiftModule${module}")
  set_property(TARGET "SwiftModule${module}" APPEND PROPERTY SOURCES ${sources})
endfunction()
 
# Add a library target for the swift compiler modules.
#
# Adds targets to compile all swift compiler modules and a target for the
# library itself.
#
function(add_swift_compiler_modules_library name)
  cmake_parse_arguments(ALS
                        ""
                        "BOOTSTRAPPING;SWIFT_EXEC"
                        "DEPENDS"
                        ${ARGN})

  set(swift_compile_options
      "-Xfrontend" "-validate-tbd-against-ir=none"
      "-Xfrontend" "-enable-cxx-interop"
      "-Xcc" "-UIBOutlet" "-Xcc" "-UIBAction" "-Xcc" "-UIBInspectable")

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND swift_compile_options "-g")
  else()
    list(APPEND swift_compile_options "-O" "-cross-module-optimization")
  endif()

  get_bootstrapping_path(build_dir ${CMAKE_CURRENT_BINARY_DIR} "${ALS_BOOTSTRAPPING}")

  set(sdk_option "")

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(deployment_version "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
    set(sdk_path "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${SWIFT_HOST_VARIANT_ARCH}_PATH}")
    set(sdk_option "-sdk" "${sdk_path}")
    if(${BOOTSTRAPPING_MODE} STREQUAL "CROSSCOMPILE-WITH-HOSTLIBS")
      # Let the cross-compiled compile don't pick up the compiled stdlib by providing
      # an (almost) empty resource dir.
      # The compiler will instead pick up the stdlib from the SDK.
      get_filename_component(swift_exec_bin_dir ${ALS_SWIFT_EXEC} DIRECTORY)
      set(sdk_option ${sdk_option} "-resource-dir" "${swift_exec_bin_dir}/../bootstrapping0/lib/swift")
    endif()
    if(NOT EXISTS "${sdk_path}/usr/include/c++")
      # Darwin SDKs in Xcode 12 or older do not include libc++, which prevents clang from finding libc++ when invoked
      # from ClangImporter. This results in build errors. To workaround this, let's explicitly pass the path to libc++
      # to clang.
      message(WARNING "Building with an outdated Darwin SDK: libc++ missing from the ${SWIFT_HOST_VARIANT_SDK} SDK. Will use libc++ from the toolchain.")
      get_filename_component(absolute_libcxx_path "${CMAKE_C_COMPILER}/../../include/c++/v1" REALPATH)
      if (EXISTS "${absolute_libcxx_path}")
        set(sdk_option ${sdk_option} "-Xcc" "-isystem" "-Xcc" "${absolute_libcxx_path}")
      else()
        message(ERROR "libc++ not found in the toolchain.")
      endif()
    endif()
  elseif(${BOOTSTRAPPING_MODE} STREQUAL "CROSSCOMPILE")
    set(sdk_option "-sdk" "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${SWIFT_HOST_VARIANT_ARCH}_PATH}")
    get_filename_component(swift_exec_bin_dir ${ALS_SWIFT_EXEC} DIRECTORY)
    set(sdk_option ${sdk_option} "-resource-dir" "${swift_exec_bin_dir}/../lib/swift")
  endif()
  get_versioned_target_triple(target ${SWIFT_HOST_VARIANT_SDK}
      ${SWIFT_HOST_VARIANT_ARCH} "${deployment_version}")

  set(all_obj_files)
  set(all_module_targets)
  set(syntaxparse_obj_files)
  set(syntaxparse_module_targets)
  get_property(modules GLOBAL PROPERTY "swift_compiler_modules")
  foreach(module ${modules})

    set(module_target "SwiftModule${module}")
    get_target_property(module ${module_target} "module_name")
    get_target_property(sources ${module_target} SOURCES)
    get_target_property(dependencies ${module_target} "module_depends")
    get_target_property(add_to_syntaxparse ${module_target} "add_to_syntaxparse")
    set(deps, "")
    if (dependencies)
      foreach(dep_module ${dependencies})
        if (DEFINED "${dep_module}_dep_target")
          # We have to add the module target for the ordering dependency
          # and the output file for the file dependency (otherwise the dependent
          # module wouldn't be rebuilt if the current module changes)
          list(APPEND deps "${${dep_module}_dep_target}" "${build_dir}/${dep_module}.o")
        else()
          message(FATAL_ERROR "module dependency ${module} -> ${dep_module} not found. Make sure to add modules in dependency order")
        endif()
      endforeach()
    endif()

    set(module_obj_file "${build_dir}/${module}.o")
    set(module_file "${build_dir}/${module}.swiftmodule")
    set_property(TARGET ${module_target} PROPERTY "module_file" "${module_file}")

    set(all_obj_files ${all_obj_files} ${module_obj_file})
    if (add_to_syntaxparse)
      set(syntaxparse_obj_files ${syntaxparse_obj_files} ${module_obj_file})
    endif()

    # Compile the module into an object file
    add_custom_command_target(dep_target OUTPUT ${module_obj_file}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      DEPENDS ${sources} ${deps} ${ALS_DEPENDS}
      COMMAND ${ALS_SWIFT_EXEC} "-c" "-o" ${module_obj_file}
              ${sdk_option}
              "-target" ${target}
              "-module-name" ${module} "-emit-module"
              "-emit-module-path" "${build_dir}/${module}.swiftmodule"
              "-parse-as-library" ${sources}
              "-wmo" ${swift_compile_options}
              # Bridging modules and headers.
              "-Xcc" "-I" "-Xcc" "${SWIFT_SOURCE_DIR}/include"
              # Generated C headers.
              "-Xcc" "-I" "-Xcc" "${CMAKE_BINARY_DIR}/include"
              # Generated swift modules.
              "-I" "${build_dir}"
      COMMENT "Building swift module ${module}")

    set("${module}_dep_target" ${dep_target})
    set(all_module_targets ${all_module_targets} ${dep_target})
    if (add_to_syntaxparse)
      set(syntaxparse_module_targets ${syntaxparse_module_targets} ${dep_target})
    endif()
  endforeach()

  # Create a static library containing all module object files.
  if (XCODE)
    # Xcode does not compile libraries that contain only object files.
    # Therefore, it fails to create the static library. As a workaround,
    # we add an empty source file force_lib.c to the target.
    set(all_obj_files force_lib.c ${all_obj_files})
  endif()
  add_library(${name} STATIC ${all_obj_files})
  add_dependencies(${name} ${all_module_targets})
  set_target_properties(${name} PROPERTIES LINKER_LANGUAGE CXX)
  set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS ${name})

  if (XCODE)
    set(syntaxparse_obj_files force_lib.c ${syntaxparse_obj_files})
  endif()
  add_library("${name}_SwiftSyntax" STATIC ${syntaxparse_obj_files})
  add_dependencies("${name}_SwiftSyntax" ${syntaxparse_module_targets})
  set_target_properties("${name}_SwiftSyntax" PROPERTIES LINKER_LANGUAGE CXX)
  set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS "${name}_SwiftSyntax")

endfunction()


# A dummy library if swift in the compiler is disabled
add_swift_host_library(swiftCompilerStub OBJECT stubs.cpp)

if (NOT BOOTSTRAPPING_MODE)

  add_library(swiftCompilerModules ALIAS swiftCompilerStub)
  add_library(swiftCompilerModules_SwiftSyntax ALIAS swiftCompilerStub)

else()
  # Note: "Swift" is not added intentionally here, because it would break
  # the bootstrapping build in case no swift toolchain is installed on the host.
  project(SwiftInTheCompiler LANGUAGES C CXX)

  add_subdirectory(Sources)

  if(${BOOTSTRAPPING_MODE} MATCHES "HOSTTOOLS|CROSSCOMPILE")

    if (NOT SWIFT_EXEC_FOR_SWIFT_MODULES)
      message(FATAL_ERROR "Need a swift toolchain building swift compiler sources")
    endif()

    if(${BOOTSTRAPPING_MODE} STREQUAL "HOSTTOOLS")
      if(NOT SWIFT_EXEC_FOR_SWIFT_MODULES STREQUAL CMAKE_Swift_COMPILER)
        message(FATAL_ERROR "The Swift compiler (${CMAKE_Swift_COMPILER}) differs from the Swift compiler in SWIFT_NATIVE_SWIFT_TOOLS_PATH (${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swiftc).")
      endif()

      set(min_supported_swift_version 5.5)
      if(CMAKE_Swift_COMPILER_VERSION VERSION_LESS "${min_supported_swift_version}")
        message(FATAL_ERROR
            "Outdated Swift compiler: building with host tools requires Swift ${min_supported_swift_version} or newer. "
            "Please update your Swift toolchain or switch BOOTSTRAPPING_MODE to BOOTSTRAPPING(-WITH-HOSTLIBS)? or OFF.")
      endif()
    endif()

    add_swift_compiler_modules_library(swiftCompilerModules
      SWIFT_EXEC "${SWIFT_EXEC_FOR_SWIFT_MODULES}")

  elseif(${BOOTSTRAPPING_MODE} MATCHES "BOOTSTRAPPING.*")

    set(b0_deps swift-frontend-bootstrapping0 symlink-headers-bootstrapping0)
    set(b1_deps swift-frontend-bootstrapping1 symlink-headers-bootstrapping1)
    if(${BOOTSTRAPPING_MODE} STREQUAL "BOOTSTRAPPING")
      list(APPEND b0_deps swiftCore-bootstrapping0)
      list(APPEND b1_deps swiftCore-bootstrapping1)
      if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        list(APPEND b0_deps swiftSwiftOnoneSupport-bootstrapping0)
        list(APPEND b1_deps swiftSwiftOnoneSupport-bootstrapping1)
      endif()
      if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
        list(APPEND b0_deps swiftDarwin-bootstrapping0)
        list(APPEND b1_deps swiftDarwin-bootstrapping1)
      endif()
      if(${SWIFT_HOST_VARIANT_SDK} IN_LIST SWIFT_LIBSTDCXX_PLATFORMS)
        list(APPEND b0_deps copy-libstdcxx-modulemap-bootstrapping0 copy-libstdcxx-header-bootstrapping0)
        list(APPEND b1_deps copy-libstdcxx-modulemap-bootstrapping1 copy-libstdcxx-header-bootstrapping1)
      endif()
    endif()
    if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
      set(platform ${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR})
      set(compatibility_libs
          "swiftCompatibility50-${platform}"
          "swiftCompatibility51-${platform}"
          "swiftCompatibilityDynamicReplacements-${platform}")

      list(APPEND b0_deps ${compatibility_libs})
      list(APPEND b1_deps ${compatibility_libs})
    endif()


    # Bootstrapping - stage 1, using the compiler from level 0

    add_swift_compiler_modules_library(swiftCompilerModules-bootstrapping1
      SWIFT_EXEC $<TARGET_FILE_DIR:swift-frontend-bootstrapping0>/swiftc${CMAKE_EXECUTABLE_SUFFIX}
      DEPENDS ${b0_deps}
      BOOTSTRAPPING 1)

    # The final build, using the compiler from stage 1

    add_swift_compiler_modules_library(swiftCompilerModules
        SWIFT_EXEC $<TARGET_FILE_DIR:swift-frontend-bootstrapping1>/swiftc${CMAKE_EXECUTABLE_SUFFIX}
        DEPENDS ${b1_deps})

    if(BOOTSTRAPPING_MODE STREQUAL "BOOTSTRAPPING-WITH-HOSTLIBS")
      file(GLOB module_dirs "${CMAKE_BINARY_DIR}/bootstrapping*/lib/swift/macosx/*.swiftmodule")
      foreach(module_dir ${module_dirs})
        message(WARNING "${module_dir} found from a previous 'bootstrapping' build: removing")
        file(REMOVE_RECURSE "${module_dir}")
      endforeach()
    endif()
  else()
    message(FATAL_ERROR "Unknown BOOTSTRAPPING_MODE '${BOOTSTRAPPING_MODE}'")
  endif()

endif()
