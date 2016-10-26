
include(SwiftSharedCMakeConfig)

set(SWIFT_BUILD_EXTERNAL_STDLIB TRUE)

function(get_ext_project_build_command out_var target)
  if (CMAKE_GENERATOR MATCHES "Make")
    # Use special command for Makefiles to support parallelism.
    set(${out_var} "$(MAKE)" "${target}" PARENT_SCOPE)
  else()
    set(${out_var} ${CMAKE_COMMAND} --build . --target ${target}
      --config $<CONFIGURATION> PARENT_SCOPE)
  endif()
endfunction()

macro(configure_swift_stdlib)
  if (SWIFT_BUILD_EXTERNAL_STDLIB)
    _external_configure_swift_stdlib()
  else()
    _in_tree_configure_swift_stdlib()
  endif()
endmacro()

macro(_in_tree_configure_swift_stdlib)
  if(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
    message(WARNING "Building the swift runtime using the host compiler, and not the just-built clang.")
  else()
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/bin/clang++")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/bin/clang")
    set(CMAKE_CXX_COMPILER_ARG1 "")
    set(CMAKE_C_COMPILER_ARG1 "")
  endif()

  set(SWIFT_STDLIB_LIBRARY_BUILD_TYPES)
  if(SWIFT_BUILD_DYNAMIC_STDLIB)
    list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)
  endif()
  if(SWIFT_BUILD_STATIC_STDLIB)
    list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES STATIC)
  endif()

  add_custom_target(swift-stdlib-all)
  foreach(SDK ${SWIFT_SDKS})
    add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
    add_custom_target("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
    foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
      add_custom_target("swift-stdlib${VARIANT_SUFFIX}")
      add_custom_target("swift-test-stdlib${VARIANT_SUFFIX}")
      add_dependencies(swift-stdlib-all "swift-stdlib${VARIANT_SUFFIX}")
      add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-stdlib${VARIANT_SUFFIX}")
      add_dependencies("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-test-stdlib${VARIANT_SUFFIX}")
    endforeach()
  endforeach()
  add_custom_target(swift-stdlib
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")
  add_custom_target(swift-test-stdlib ALL
    DEPENDS "swift-test-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")

  if(SWIFT_STDLIB_ENABLE_RESILIENCE)
    set(STDLIB_SIL_SERIALIZE_ALL)
  else()
    if(SWIFT_STDLIB_SIL_SERIALIZE_ALL)
      set(STDLIB_SIL_SERIALIZE_ALL "-Xfrontend" "-sil-serialize-all")
    else()
      set(STDLIB_SIL_SERIALIZE_ALL)
    endif()
  endif()

  add_subdirectory(public)
  add_subdirectory(internal)
  add_subdirectory(private)
endmacro()

include(ExternalProject)

function(_external_configure_swift_stdlib)
  if(CMAKE_VERSION VERSION_GREATER 3.3.20150708)
    set(cmake_3_4_USES_TERMINAL_OPTIONS
      USES_TERMINAL_CONFIGURE 1
      USES_TERMINAL_BUILD 1
      USES_TERMINAL_INSTALL 1
      )
  endif()

  if(CMAKE_VERSION VERSION_GREATER 3.1.20141117)
    set(cmake_3_2_USES_TERMINAL USES_TERMINAL)
  endif()

  # Prepopulate a cache containing platform independent variables for all of our
  # targets.
  set(SWIFT_STDLIB_GLOBAL_CMAKE_CACHE)
  get_cmake_property(variableNames VARIABLES)
  foreach(variableName ${variableNames})
    if(variableName MATCHES "^SWIFT")
      set(SWIFT_STDLIB_GLOBAL_CMAKE_CACHE "${SWIFT_STDLIB_GLOBAL_CMAKE_CACHE}set(${variableName} \"${${variableName}}\")\n")
    endif()
  endforeach()

  get_property(SWIFT_EXPORTS GLOBAL PROPERTY SWIFT_EXPORTS)
  export(TARGETS ${SWIFT_EXPORTS} FILE ${CMAKE_BINARY_DIR}/SwiftTargets.cmake)
  set(SWIFT_STDLIB_GLOBAL_CMAKE_CACHE "${SWIFT_STDLIB_GLOBAL_CMAKE_CACHE}include(\"${CMAKE_BINARY_DIR}/SwiftTargets.cmake\")\n")

  set(SWIFT_STDLIB_GLOBAL_CONFIG_CACHE_PATH
    ${CMAKE_BINARY_DIR}/SwiftStdlibGlobalConfig.cmake)
  file(WRITE "${SWIFT_STDLIB_GLOBAL_CONFIG_CACHE_PATH}" "${SWIFT_STDLIB_GLOBAL_CMAKE_CACHE}")

  add_custom_target(swift-stdlib-all)
  add_custom_target(swift-stdlib-clear)
  foreach(SDK ${SWIFT_SDKS})
    set(SDK_LIBSUBDIR_SUFFIX "${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
    add_custom_target("swift-stdlib-${SDK_LIBSUBDIR_SUFFIX}")
    add_custom_target("swift-test-stdlib-${SDK_LIBSUBDIR_SUFFIX}")
    add_custom_target("swift-stdlib-clear-${SDK_LIBSUBDIR_SUFFIX}")

    foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SDK_LIBSUBDIR_SUFFIX}-${ARCH}")

      # Add swift-stdlib as an external project.
      set(SWIFT_STDLIB_PREFIX ${CMAKE_BINARY_DIR}/stdlib/swift-stdlib${VARIANT_SUFFIX})
      set(SWIFT_STDLIB_SRC_ROOT ${SWIFT_SOURCE_DIR}/stdlib)

      set(STAMP_DIR ${CMAKE_BINARY_DIR}/stdlib/swift-stdlib${VARIANT_SUFFIX}-stamps/)
      set(BINARY_DIR ${CMAKE_BINARY_DIR}/stdlib/swift-stdlib${VARIANT_SUFFIX}-bins/)

      ExternalProject_Add(swift-stdlib${VARIANT_SUFFIX}
        DEPENDS swift clang
        PREFIX ${SWIFT_STDLIB_PREFIX}
        SOURCE_DIR ${SWIFT_STDLIB_SRC_ROOT}
        STAMP_DIR ${STAMP_DIR}
        BINARY_DIR ${BINARY_DIR}
        CMAKE_ARGS ${CLANG_SWIFT_STDLIB_CMAKE_ARGS}
        -DCMAKE_C_COMPILER=${LLVM_RUNTIME_OUTPUT_INTDIR}/bin/clang
        -DCMAKE_CXX_COMPILER=${LLVM_RUNTIME_OUTPUT_INTDIR}/bin/clang++
        -DCMAKE_BUILD_TYPE=${SWIFT_STDLIB_BUILD_TYPE}
        -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
        -DLLVM_LIT_ARGS=${LLVM_LIT_ARGS}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
        -DSWIFT_STDLIB_GLOBAL_CACHE_PATH=${SWIFT_STDLIB_GLOBAL_CONFIG_CACHE_PATH}
        -DSWIFT_STDLIB_PRIMARY_VARIANT_SDK=${SDK}
        -DSWIFT_STDLIB_PRIMARY_VARIANT_ARCH=${ARCH}
        INSTALL_COMMAND ""
        STEP_TARGETS configure build
        ${cmake_3_4_USES_TERMINAL_OPTIONS}
        )

      add_dependencies("swift-stdlib-all" "swift-stdlib${VARIANT_SUFFIX}")
      add_dependencies("swift-stdlib-${SDK_LIBSUBDIR_SUFFIX}"
        "swift-stdlib${VARIANT_SUFFIX}")

      add_custom_target("swift-test-stdlib${VARIANT_SUFFIX}")
      add_dependencies("swift-test-stdlib-${SDK_LIBSUBDIR_SUFFIX}"
        "swift-test-stdlib${VARIANT_SUFFIX}")

      get_ext_project_build_command(run_clean_swift_stdlib${VARIANT_SUFFIX} clean)
      ExternalProject_Add_Step(swift-stdlib${VARIANT_SUFFIX} clean
        COMMAND ${run_clean_swift_stdlib${VARIANT_SUFFIX}}
        COMMENT "Cleaning swift-stdlib${VARIANT_SUFFIX}..."
        DEPENDEES configure
        DEPENDERS build
        DEPENDS swift
        WORKING_DIRECTORY ${BINARY_DIR}
        )

      add_custom_target("swift-stdlib${VARIANT_SUFFIX}-clear"
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${STAMP_DIR}
        COMMENT "Clobberring swift-stdlib${VARIANT_SUFFIX} build and stamp directories"
        )
      add_dependencies(swift-stdlib-clear "swift-stdlib${VARIANT_SUFFIX}-clear")
      add_dependencies("swift-stdlib-clear-${SDK_LIBSUBDIR_SUFFIX}" "swift-stdlib${VARIANT_SUFFIX}-clear")
    endforeach()
  endforeach()
endfunction()
