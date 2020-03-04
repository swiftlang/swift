include(macCatalystUtils)
include(SwiftList)
include(SwiftXcodeSupport)
include(SwiftWindowsSupport)
include(SwiftAndroidSupport)

# SWIFTLIB_DIR is the directory in the build tree where Swift resource files
# should be placed.  Note that $CMAKE_CFG_INTDIR expands to "." for
# single-configuration builds.
set(SWIFTLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift")
set(SWIFTSTATICLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift_static")

function(add_dependencies_multiple_targets)
  cmake_parse_arguments(
      ADMT # prefix
      "" # options
      "" # single-value args
      "TARGETS;DEPENDS" # multi-value args
      ${ARGN})
  precondition(ADMT_UNPARSED_ARGUMENTS NEGATE MESSAGE "unrecognized arguments: ${ADMT_UNPARSED_ARGUMENTS}")

  if(NOT "${ADMT_DEPENDS}" STREQUAL "")
    foreach(target ${ADMT_TARGETS})
      add_dependencies("${target}" ${ADMT_DEPENDS})
    endforeach()
  endif()
endfunction()

function(_compute_lto_flag option out_var)
  string(TOLOWER "${option}" lowercase_option)
  if (lowercase_option STREQUAL "full")
    set(${out_var} "-flto=full" PARENT_SCOPE)
  elseif (lowercase_option STREQUAL "thin")
    set(${out_var} "-flto=thin" PARENT_SCOPE)
  endif()
endfunction()

function(_set_target_prefix_and_suffix target kind sdk)
  precondition(target MESSAGE "target is required")
  precondition(kind MESSAGE "kind is required")
  precondition(sdk MESSAGE "sdk is required")

  if(${sdk} STREQUAL ANDROID)
    if(${kind} STREQUAL STATIC)
      set_target_properties(${target} PROPERTIES PREFIX "lib" SUFFIX ".a")
    elseif(${kind} STREQUAL SHARED)
      set_target_properties(${target} PROPERTIES PREFIX "lib" SUFFIX ".so")
    endif()
  elseif(${sdk} STREQUAL WINDOWS)
    if(${kind} STREQUAL STATIC)
      set_target_properties(${target} PROPERTIES PREFIX "" SUFFIX ".lib")
    elseif(${kind} STREQUAL SHARED)
      set_target_properties(${target} PROPERTIES PREFIX "" SUFFIX ".dll")
    endif()
  endif()
endfunction()

function(is_darwin_based_sdk sdk_name out_var)
  if ("${sdk_name}" STREQUAL "OSX" OR
      "${sdk_name}" STREQUAL "IOS" OR
      "${sdk_name}" STREQUAL "IOS_SIMULATOR" OR
      "${sdk_name}" STREQUAL "TVOS" OR
      "${sdk_name}" STREQUAL "TVOS_SIMULATOR" OR
      "${sdk_name}" STREQUAL "WATCHOS" OR
      "${sdk_name}" STREQUAL "WATCHOS_SIMULATOR")
    set(${out_var} TRUE PARENT_SCOPE)
  else()
    set(${out_var} FALSE PARENT_SCOPE)
  endif()
endfunction()

# Usage:
# _add_variant_c_compile_link_flags(
#   SDK sdk
#   ARCH arch
#   BUILD_TYPE build_type
#   ENABLE_LTO enable_lto
#   ANALYZE_CODE_COVERAGE analyze_code_coverage
#   RESULT_VAR_NAME result_var_name
#   DEPLOYMENT_VERSION_OSX version # If provided, overrides the default value of the OSX deployment target set by the Swift project for this compilation only.
#   DEPLOYMENT_VERSION_MACCATALYST version
#   DEPLOYMENT_VERSION_IOS version
#   DEPLOYMENT_VERSION_TVOS version
#   DEPLOYMENT_VERSION_WATCHOS version
#
# )
function(_add_variant_c_compile_link_flags)
  set(oneValueArgs SDK ARCH BUILD_TYPE RESULT_VAR_NAME ENABLE_LTO ANALYZE_CODE_COVERAGE
    DEPLOYMENT_VERSION_OSX DEPLOYMENT_VERSION_MACCATALYST DEPLOYMENT_VERSION_IOS DEPLOYMENT_VERSION_TVOS DEPLOYMENT_VERSION_WATCHOS
    MACCATALYST_BUILD_FLAVOR
  )
  cmake_parse_arguments(CFLAGS
    ""
    "${oneValueArgs}"
    ""
    ${ARGN})

  get_maccatalyst_build_flavor(maccatalyst_build_flavor
      "${CFLAGS_SDK}" "${CFLAGS_MACCATALYST_BUILD_FLAVOR}")

  set(result ${${CFLAGS_RESULT_VAR_NAME}})

  is_darwin_based_sdk("${CFLAGS_SDK}" IS_DARWIN)
  if(IS_DARWIN)
    # Check if there's a specific OS deployment version needed for this invocation
    if("${CFLAGS_SDK}" STREQUAL "OSX")
      if(DEFINED maccatalyst_build_flavor)
        set(DEPLOYMENT_VERSION ${CFLAGS_DEPLOYMENT_VERSION_MACCATALYST})
      else()
        set(DEPLOYMENT_VERSION ${CFLAGS_DEPLOYMENT_VERSION_OSX})
      endif()
    elseif("${CFLAGS_SDK}" STREQUAL "IOS" OR "${CFLAGS_SDK}" STREQUAL "IOS_SIMULATOR")
      set(DEPLOYMENT_VERSION ${CFLAGS_DEPLOYMENT_VERSION_IOS})
    elseif("${CFLAGS_SDK}" STREQUAL "TVOS" OR "${CFLAGS_SDK}" STREQUAL "TVOS_SIMULATOR")
      set(DEPLOYMENT_VERSION ${CFLAGS_DEPLOYMENT_VERSION_TVOS})
    elseif("${CFLAGS_SDK}" STREQUAL "WATCHOS" OR "${CFLAGS_SDK}" STREQUAL "WATCHOS_SIMULATOR")
      set(DEPLOYMENT_VERSION ${CFLAGS_DEPLOYMENT_VERSION_WATCHOS})
    endif()

    if("${DEPLOYMENT_VERSION}" STREQUAL "")
      set(DEPLOYMENT_VERSION "${SWIFT_SDK_${CFLAGS_SDK}_DEPLOYMENT_VERSION}")
    endif()
  endif()

  # MSVC, clang-cl, gcc don't understand -target.
  if(CMAKE_C_COMPILER_ID MATCHES "^Clang|AppleClang$" AND
      NOT SWIFT_COMPILER_IS_MSVC_LIKE)
    get_target_triple(target target_variant "${CFLAGS_SDK}" "${CFLAGS_ARCH}"
      MACCATALYST_BUILD_FLAVOR "${maccatalyst_build_flavor}"
      DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")
    list(APPEND result "-target" "${target}")
    if(target_variant)
      list(APPEND result "-target-variant" "${target_variant}")
    endif()
  endif()

  set(_sysroot "${SWIFT_SDK_${CFLAGS_SDK}_ARCH_${CFLAGS_ARCH}_PATH}")
  if(IS_DARWIN)
    list(APPEND result "-isysroot" "${_sysroot}")
  elseif(NOT SWIFT_COMPILER_IS_MSVC_LIKE AND NOT "${_sysroot}" STREQUAL "/")
    list(APPEND result "--sysroot=${_sysroot}")
  endif()

  if("${CFLAGS_SDK}" STREQUAL "ANDROID")
    # lld can handle targeting the android build.  However, if lld is not
    # enabled, then fallback to the linker included in the android NDK.
    if(NOT SWIFT_ENABLE_LLD_LINKER)
      swift_android_tools_path(${CFLAGS_ARCH} tools_path)
      list(APPEND result "-B" "${tools_path}")
    endif()
  endif()

  if(IS_DARWIN)
    # We collate -F with the framework path to avoid unwanted deduplication
    # of options by target_compile_options -- this way no undesired
    # side effects are introduced should a new search path be added.
    list(APPEND result
      "-arch" "${CFLAGS_ARCH}"
      "-F${SWIFT_SDK_${CFLAGS_SDK}_PATH}/../../../Developer/Library/Frameworks")

    set(add_explicit_version TRUE)

    # iOS-like and zippered libraries get their deployment version from the
    # target triple
    if(maccatalyst_build_flavor STREQUAL "ios-like" OR
        maccatalyst_build_flavor STREQUAL "zippered")
      set(add_explicit_version FALSE)
    endif()

    if(add_explicit_version)
      list(APPEND result
        "-m${SWIFT_SDK_${CFLAGS_SDK}_VERSION_MIN_NAME}-version-min=${DEPLOYMENT_VERSION}")
     endif()
  endif()

  if(CFLAGS_ANALYZE_CODE_COVERAGE)
    list(APPEND result "-fprofile-instr-generate"
                       "-fcoverage-mapping")
  endif()

  _compute_lto_flag("${CFLAGS_ENABLE_LTO}" _lto_flag_out)
  if (_lto_flag_out)
    list(APPEND result "${_lto_flag_out}")
  endif()

  set("${CFLAGS_RESULT_VAR_NAME}" "${result}" PARENT_SCOPE)
endfunction()


function(_add_variant_c_compile_flags)
  set(oneValueArgs SDK ARCH BUILD_TYPE ENABLE_ASSERTIONS ANALYZE_CODE_COVERAGE
    DEPLOYMENT_VERSION_OSX DEPLOYMENT_VERSION_MACCATALYST DEPLOYMENT_VERSION_IOS DEPLOYMENT_VERSION_TVOS DEPLOYMENT_VERSION_WATCHOS
    RESULT_VAR_NAME ENABLE_LTO
    MACCATALYST_BUILD_FLAVOR)
  cmake_parse_arguments(CFLAGS
    ""
    "${oneValueArgs}"
    ""
    ${ARGN})

  set(result ${${CFLAGS_RESULT_VAR_NAME}})

  _add_variant_c_compile_link_flags(
    SDK "${CFLAGS_SDK}"
    ARCH "${CFLAGS_ARCH}"
    BUILD_TYPE "${CFLAGS_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${CFLAGS_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${CFLAGS_ENABLE_LTO}"
    ANALYZE_CODE_COVERAGE FALSE
    DEPLOYMENT_VERSION_OSX "${CFLAGS_DEPLOYMENT_VERSION_OSX}"
    DEPLOYMENT_VERSION_MACCATALYST "${CFLAGS_DEPLOYMENT_VERSION_MACCATALYST}"
    DEPLOYMENT_VERSION_IOS "${CFLAGS_DEPLOYMENT_VERSION_IOS}"
    DEPLOYMENT_VERSION_TVOS "${CFLAGS_DEPLOYMENT_VERSION_TVOS}"
    DEPLOYMENT_VERSION_WATCHOS "${CFLAGS_DEPLOYMENT_VERSION_WATCHOS}"
    RESULT_VAR_NAME result
    MACCATALYST_BUILD_FLAVOR "${CFLAGS_MACCATALYST_BUILD_FLAVOR}")

  is_build_type_optimized("${CFLAGS_BUILD_TYPE}" optimized)
  if(optimized)
    list(APPEND result "-O2")

    # Omit leaf frame pointers on x86 production builds (optimized, no debug
    # info, and no asserts).
    is_build_type_with_debuginfo("${CFLAGS_BUILD_TYPE}" debug)
    if(NOT debug AND NOT CFLAGS_ENABLE_ASSERTIONS)
      if("${CFLAGS_ARCH}" STREQUAL "i386" OR "${CFLAGS_ARCH}" STREQUAL "i686")
        if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
          list(APPEND result "-momit-leaf-frame-pointer")
        else()
          list(APPEND result "/Oy")
        endif()
      endif()
    endif()
  else()
    if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
      list(APPEND result "-O0")
    else()
      list(APPEND result "/Od")
    endif()
  endif()

  # CMake automatically adds the flags for debug info if we use MSVC/clang-cl.
  if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
    is_build_type_with_debuginfo("${CFLAGS_BUILD_TYPE}" debuginfo)
    if(debuginfo)
      _compute_lto_flag("${CFLAGS_ENABLE_LTO}" _lto_flag_out)
      if(_lto_flag_out)
        list(APPEND result "-gline-tables-only")
      else()
        list(APPEND result "-g")
      endif()
    else()
      list(APPEND result "-g0")
    endif()
  endif()

  if("${CFLAGS_SDK}" STREQUAL "WINDOWS")
    # MSVC/clang-cl don't support -fno-pic or -fms-compatibility-version.
    if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
      list(APPEND result -fno-pic)
      list(APPEND result "-fms-compatibility-version=1900")
    endif()

    list(APPEND result "-DLLVM_ON_WIN32")
    list(APPEND result "-D_CRT_SECURE_NO_WARNINGS")
    list(APPEND result "-D_CRT_NONSTDC_NO_WARNINGS")
    if(NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
      list(APPEND result "-D_CRT_USE_BUILTIN_OFFSETOF")
    endif()
    # TODO(compnerd) permit building for different families
    list(APPEND result "-D_CRT_USE_WINAPI_FAMILY_DESKTOP_APP")
    if("${CFLAGS_ARCH}" MATCHES arm)
      list(APPEND result "-D_ARM_WINAPI_PARTITION_DESKTOP_SDK_AVAILABLE")
    endif()
    list(APPEND result "-D_MT")
    # TODO(compnerd) handle /MT
    list(APPEND result "-D_DLL")
    # NOTE: We assume that we are using VS 2015 U2+
    list(APPEND result "-D_ENABLE_ATOMIC_ALIGNMENT_FIX")
    # NOTE: We use over-aligned values for the RefCount side-table
    # (see revision d913eefcc93f8c80d6d1a6de4ea898a2838d8b6f)
    # This is required to build with VS2017 15.8+
    list(APPEND result "-D_ENABLE_EXTENDED_ALIGNED_STORAGE=1")

    # msvcprt's std::function requires RTTI, but we do not want RTTI data.
    # Emulate /GR-.
    # TODO(compnerd) when moving up to VS 2017 15.3 and newer, we can disable
    # RTTI again
    if(SWIFT_COMPILER_IS_MSVC_LIKE)
      list(APPEND result /GR-)
    else()
      list(APPEND result -frtti)
      list(APPEND result -Xclang;-fno-rtti-data)
    endif()

    # NOTE: VS 2017 15.3 introduced this to disable the static components of
    # RTTI as well.  This requires a newer SDK though and we do not have
    # guarantees on the SDK version currently.
    list(APPEND result "-D_HAS_STATIC_RTTI=0")

    # NOTE(compnerd) workaround LLVM invoking `add_definitions(-D_DEBUG)` which
    # causes failures for the runtime library when cross-compiling due to
    # undefined symbols from the standard library.
    if(NOT CMAKE_BUILD_TYPE STREQUAL Debug)
      list(APPEND result "-U_DEBUG")
    endif()
  endif()

  if(${CFLAGS_SDK} STREQUAL ANDROID)
    if(${CFLAGS_ARCH} STREQUAL x86_64)
      # NOTE(compnerd) Android NDK 21 or lower will generate library calls to
      # `__sync_val_compare_and_swap_16` rather than lowering to the CPU's
      # `cmpxchg16b` instruction as the `cx16` feature is disabled due to a bug
      # in Clang.  This is being fixed in the current master Clang and will
      # hopefully make it into Clang 9.0.  In the mean time, workaround this in
      # the build.
      if(CMAKE_C_COMPILER_ID MATCHES Clang AND CMAKE_C_COMPILER_VERSION
          VERSION_LESS 9.0.0)
        list(APPEND result -mcx16)
      endif()
    endif()
  endif()

  if(CFLAGS_ENABLE_ASSERTIONS)
    list(APPEND result "-UNDEBUG")
  else()
    list(APPEND result "-DNDEBUG")
  endif()
  
  if(SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS)
    list(APPEND result "-DSWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS")
  endif()

  if(CFLAGS_ANALYZE_CODE_COVERAGE)
    list(APPEND result "-fprofile-instr-generate"
                       "-fcoverage-mapping")
  endif()

  if((CFLAGS_ARCH STREQUAL "armv7" OR CFLAGS_ARCH STREQUAL "aarch64") AND
     (CFLAGS_SDK STREQUAL "LINUX" OR CFLAGS_SDK STREQUAL "ANDROID"))
     list(APPEND result -funwind-tables)
  endif()

  if("${CFLAGS_SDK}" STREQUAL "ANDROID")
    list(APPEND result -nostdinc++)
    swift_android_libcxx_include_paths(CFLAGS_CXX_INCLUDES)
    swift_android_include_for_arch("${CFLAGS_ARCH}" "${CFLAGS_ARCH}_INCLUDE")
    foreach(path IN LISTS CFLAGS_CXX_INCLUDES ${CFLAGS_ARCH}_INCLUDE)
      list(APPEND result "SHELL:${CMAKE_INCLUDE_SYSTEM_FLAG_C}${path}")
    endforeach()
    list(APPEND result "-D__ANDROID_API__=${SWIFT_ANDROID_API_LEVEL}")
  endif()

  if("${CFLAGS_SDK}" STREQUAL "LINUX")
    if(${CFLAGS_ARCH} STREQUAL x86_64)
      # this is the minimum architecture that supports 16 byte CAS, which is necessary to avoid a dependency to libatomic
      list(APPEND result "-march=core2")
    endif()
  endif()

  set("${CFLAGS_RESULT_VAR_NAME}" "${result}" PARENT_SCOPE)
endfunction()

function(_add_variant_link_flags)
  set(oneValueArgs SDK ARCH BUILD_TYPE ENABLE_ASSERTIONS ANALYZE_CODE_COVERAGE
  DEPLOYMENT_VERSION_OSX DEPLOYMENT_VERSION_MACCATALYST DEPLOYMENT_VERSION_IOS DEPLOYMENT_VERSION_TVOS DEPLOYMENT_VERSION_WATCHOS
  RESULT_VAR_NAME ENABLE_LTO LTO_OBJECT_NAME LINK_LIBRARIES_VAR_NAME LIBRARY_SEARCH_DIRECTORIES_VAR_NAME
  MACCATALYST_BUILD_FLAVOR
  )
  cmake_parse_arguments(LFLAGS
    ""
    "${oneValueArgs}"
    ""
    ${ARGN})

  precondition(LFLAGS_SDK MESSAGE "Should specify an SDK")
  precondition(LFLAGS_ARCH MESSAGE "Should specify an architecture")

  set(result ${${LFLAGS_RESULT_VAR_NAME}})
  set(link_libraries ${${LFLAGS_LINK_LIBRARIES_VAR_NAME}})
  set(library_search_directories ${${LFLAGS_LIBRARY_SEARCH_DIRECTORIES_VAR_NAME}})

  _add_variant_c_compile_link_flags(
    SDK "${LFLAGS_SDK}"
    ARCH "${LFLAGS_ARCH}"
    BUILD_TYPE "${LFLAGS_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${LFLAGS_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${LFLAGS_ENABLE_LTO}"
    ANALYZE_CODE_COVERAGE "${LFLAGS_ANALYZE_CODE_COVERAGE}"
    DEPLOYMENT_VERSION_OSX "${LFLAGS_DEPLOYMENT_VERSION_OSX}"
    DEPLOYMENT_VERSION_MACCATALYST "${LFLAGS_DEPLOYMENT_VERSION_MACCATALYST}"
    DEPLOYMENT_VERSION_IOS "${LFLAGS_DEPLOYMENT_VERSION_IOS}"
    DEPLOYMENT_VERSION_TVOS "${LFLAGS_DEPLOYMENT_VERSION_TVOS}"
    DEPLOYMENT_VERSION_WATCHOS "${LFLAGS_DEPLOYMENT_VERSION_WATCHOS}"
    RESULT_VAR_NAME result
    MACCATALYST_BUILD_FLAVOR  "${LFLAGS_MACCATALYST_BUILD_FLAVOR}")
  if("${LFLAGS_SDK}" STREQUAL "LINUX")
    list(APPEND link_libraries "pthread" "dl")
  elseif("${LFLAGS_SDK}" STREQUAL "FREEBSD")
    list(APPEND link_libraries "pthread")
  elseif("${LFLAGS_SDK}" STREQUAL "CYGWIN")
    # No extra libraries required.
  elseif("${LFLAGS_SDK}" STREQUAL "WINDOWS")
    # We don't need to add -nostdlib using MSVC or clang-cl, as MSVC and clang-cl rely on auto-linking entirely.
    if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
      # NOTE: we do not use "/MD" or "/MDd" and select the runtime via linker
      # options. This causes conflicts.
      list(APPEND result "-nostdlib")
    endif()
    swift_windows_lib_for_arch(${LFLAGS_ARCH} ${LFLAGS_ARCH}_LIB)
    list(APPEND library_search_directories ${${LFLAGS_ARCH}_LIB})

    # NOTE(compnerd) workaround incorrectly extensioned import libraries from
    # the Windows SDK on case sensitive file systems.
    list(APPEND library_search_directories
         ${CMAKE_BINARY_DIR}/winsdk_lib_${LFLAGS_ARCH}_symlinks)
  elseif("${LFLAGS_SDK}" STREQUAL "HAIKU")
    list(APPEND link_libraries "bsd" "atomic")
    list(APPEND result "-Wl,-Bsymbolic")
  elseif("${LFLAGS_SDK}" STREQUAL "ANDROID")
    list(APPEND link_libraries "dl" "log" "atomic")
    # We need to add the math library, which is linked implicitly by libc++
    list(APPEND result "-lm")

    # link against the custom C++ library
    swift_android_cxx_libraries_for_arch(${LFLAGS_ARCH} cxx_link_libraries)
    list(APPEND link_libraries ${cxx_link_libraries})

    # link against the ICU libraries
    list(APPEND link_libraries
      ${SWIFT_ANDROID_${LFLAGS_ARCH}_ICU_I18N}
      ${SWIFT_ANDROID_${LFLAGS_ARCH}_ICU_UC})

    swift_android_lib_for_arch(${LFLAGS_ARCH} ${LFLAGS_ARCH}_LIB)
    foreach(path IN LISTS ${LFLAGS_ARCH}_LIB)
      list(APPEND library_search_directories ${path})
    endforeach()
  else()
    # If lto is enabled, we need to add the object path flag so that the LTO code
    # generator leaves the intermediate object file in a place where it will not
    # be touched. The reason why this must be done is that on OS X, debug info is
    # left in object files. So if the object file is removed when we go to
    # generate a dsym, the debug info is gone.
    if (LFLAGS_ENABLE_LTO)
      precondition(LFLAGS_LTO_OBJECT_NAME
        MESSAGE "Should specify a unique name for the lto object")
      set(lto_object_dir ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
      set(lto_object ${lto_object_dir}/${LFLAGS_LTO_OBJECT_NAME}-lto.o)
        list(APPEND result "-Wl,-object_path_lto,${lto_object}")
      endif()
  endif()

  if(NOT "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_UC}" STREQUAL "")
    get_filename_component(SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_UC_LIBDIR
      "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_UC}" DIRECTORY)
    list(APPEND library_search_directories "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_UC_LIBDIR}")
  endif()
  if(NOT "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_I18N}" STREQUAL "")
    get_filename_component(SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_I18N_LIBDIR
      "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_I18N}" DIRECTORY)
    list(APPEND library_search_directories "${SWIFT_${LFLAGS_SDK}_${LFLAGS_ARCH}_ICU_I18N_LIBDIR}")
  endif()

  if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
    # FIXME: On Apple platforms, find_program needs to look for "ld64.lld"
    find_program(LDLLD_PATH "ld.lld")
    if((SWIFT_ENABLE_LLD_LINKER AND LDLLD_PATH AND NOT APPLE) OR
       ("${LFLAGS_SDK}" STREQUAL "WINDOWS" AND
        NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "WINDOWS"))
      list(APPEND result "-fuse-ld=lld")
    elseif(SWIFT_ENABLE_GOLD_LINKER AND
        "${SWIFT_SDK_${LFLAGS_SDK}_OBJECT_FORMAT}" STREQUAL "ELF")
      if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
        list(APPEND result "-fuse-ld=gold.exe")
      else()
        list(APPEND result "-fuse-ld=gold")
      endif()
    endif()
  endif()

  # Enable dead stripping. Portions of this logic were copied from llvm's
  # `add_link_opts` function (which, perhaps, should have been used here in the
  # first place, but at this point it's hard to say whether that's feasible).
  #
  # TODO: Evaluate/enable -f{function,data}-sections --gc-sections for bfd,
  # gold, and lld.
  if(NOT CMAKE_BUILD_TYPE STREQUAL Debug)
    if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
      # See rdar://48283130: This gives 6MB+ size reductions for swift and
      # SourceKitService, and much larger size reductions for sil-opt etc.
      list(APPEND result "-Wl,-dead_strip")
    endif()
  endif()

  get_maccatalyst_build_flavor(maccatalyst_build_flavor
    "${LFLAGS_SDK}" "${LFLAGS_MACCATALYST_BUILD_FLAVOR}")

  set("${LFLAGS_RESULT_VAR_NAME}" "${result}" PARENT_SCOPE)
  set("${LFLAGS_LINK_LIBRARIES_VAR_NAME}" "${link_libraries}" PARENT_SCOPE)
  set("${LFLAGS_LIBRARY_SEARCH_DIRECTORIES_VAR_NAME}" "${library_search_directories}" PARENT_SCOPE)
endfunction()

# Add a single variant of a new Swift library.
#
# Usage:
#   _add_swift_host_library_single(
#     target
#     [SHARED]
#     [STATIC]
#     [LLVM_LINK_COMPONENTS comp1 ...]
#     source1 [source2 source3 ...])
#
# target
#   Name of the library (e.g., swiftParse).
#
# SHARED
#   Build a shared library.
#
# STATIC
#   Build a static library.
#
# LLVM_LINK_COMPONENTS
#   LLVM components this library depends on.
#
# source1 ...
#   Sources to add into this library
function(_add_swift_host_library_single target)
  set(options
        SHARED
        STATIC)
  set(single_parameter_options)
  set(multiple_parameter_options
        GYB_SOURCES
        LLVM_LINK_COMPONENTS)

  cmake_parse_arguments(ASHLS
                        "${options}"
                        "${single_parameter_options}"
                        "${multiple_parameter_options}"
                        ${ARGN})
  set(ASHLS_SOURCES ${ASHLS_UNPARSED_ARGUMENTS})

  translate_flags(ASHLS "${options}")

  if(NOT ASHLS_SHARED AND NOT ASHLS_STATIC)
    message(FATAL_ERROR "Either SHARED or STATIC must be specified")
  endif()

  # Determine the subdirectory where this library will be installed.
  set(ASHLS_SUBDIR
    "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_HOST_VARIANT_ARCH}")

  # Include LLVM Bitcode slices for iOS, Watch OS, and Apple TV OS device libraries.
  set(embed_bitcode_arg)
  if(SWIFT_EMBED_BITCODE_SECTION)
    if(SWIFT_HOST_VARIANT_SDK MATCHES "(I|TV|WATCH)OS")
      list(APPEND ASHLS_C_COMPILE_FLAGS "-fembed-bitcode")
      set(embed_bitcode_arg EMBED_BITCODE)
    endif()
  endif()

  if(XCODE)
    string(REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list(GET split_path -1 dir)
    file(GLOB_RECURSE ASHLS_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def
      ${CMAKE_CURRENT_SOURCE_DIR}/*.def)

    file(GLOB_RECURSE ASHLS_TDS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td)

    set_source_files_properties(${ASHLS_HEADERS} ${ASHLS_TDS}
      PROPERTIES
      HEADER_FILE_ONLY true)
    source_group("TableGen descriptions" FILES ${ASHLS_TDS})

    set(ASHLS_SOURCES ${ASHLS_SOURCES} ${ASHLS_HEADERS} ${ASHLS_TDS})
  endif()

  if(ASHLS_SHARED)
    set(libkind SHARED)
  elseif(ASHLS_STATIC)
    set(libkind STATIC)
  endif()

  if(ASHLS_GYB_SOURCES)
    handle_gyb_sources(
        gyb_dependency_targets
        ASHLS_GYB_SOURCES
        "${SWIFT_HOST_VARIANT_ARCH}")
      set(ASHLS_SOURCES ${ASHLS_SOURCES} ${ASHLS_GYB_SOURCES})
  endif()

  add_library("${target}" ${libkind} ${ASHLS_SOURCES})
  _set_target_prefix_and_suffix("${target}" "${libkind}" "${SWIFT_HOST_VARIANT_SDK}")

  if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "WINDOWS")
    swift_windows_include_for_arch(${SWIFT_HOST_VARIANT_ARCH} SWIFTLIB_INCLUDE)
    target_include_directories("${target}" SYSTEM PRIVATE ${SWIFTLIB_INCLUDE})
    set_target_properties(${target}
                          PROPERTIES
                            CXX_STANDARD 14)
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL WINDOWS)
    set_property(TARGET "${target}" PROPERTY NO_SONAME ON)
  endif()

  llvm_update_compile_flags(${target})

  set_output_directory(${target}
      BINARY_DIR ${SWIFT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_APPLE_PLATFORMS)
    set_target_properties("${target}"
      PROPERTIES
      INSTALL_NAME_DIR "@rpath")
  elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
    set_target_properties("${target}"
      PROPERTIES
      INSTALL_RPATH "$ORIGIN:/usr/lib/swift/linux")
  elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "CYGWIN")
    set_target_properties("${target}"
      PROPERTIES
      INSTALL_RPATH "$ORIGIN:/usr/lib/swift/cygwin")
  elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "ANDROID")
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

  # Handle linking and dependencies.
  add_dependencies_multiple_targets(
      TARGETS "${target}"
      DEPENDS
        ${gyb_dependency_targets}
        ${LLVM_COMMON_DEPENDS})

  # Call llvm_config() only for libraries that are part of the compiler.
  swift_common_llvm_config("${target}" ${ASHLS_LLVM_LINK_COMPONENTS})

  # Collect compile and link flags for the static and non-static targets.
  # Don't set PROPERTY COMPILE_FLAGS or LINK_FLAGS directly.
  set(c_compile_flags ${ASHLS_C_COMPILE_FLAGS})
  set(link_flags)

  set(library_search_subdir "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}")
  set(library_search_directories
    "${SWIFTLIB_DIR}/${ASHLS_SUBDIR}"
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${ASHLS_SUBDIR}"
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}")

  # In certain cases when building, the environment variable SDKROOT is set to override
  # where the sdk root is located in the system. If that environment variable has been
  # set by the user, respect it and add the specified SDKROOT directory to the
  # library_search_directories so we are able to link against those libraries
  if(DEFINED ENV{SDKROOT} AND EXISTS "$ENV{SDKROOT}/usr/lib/swift")
      list(APPEND library_search_directories "$ENV{SDKROOT}/usr/lib/swift")
  endif()

  _add_variant_c_compile_flags(
    SDK "${SWIFT_HOST_VARIANT_SDK}"
    ARCH "${SWIFT_HOST_VARIANT_ARCH}"
    BUILD_TYPE ${CMAKE_BUILD_TYPE}
    ENABLE_ASSERTIONS ${LLVM_ENABLE_ASSERTIONS}
    ANALYZE_CODE_COVERAGE ${SWIFT_ANALYZE_CODE_COVERAGE}
    ENABLE_LTO ${SWIFT_TOOLS_ENABLE_LTO}
    RESULT_VAR_NAME c_compile_flags
    )

  if(SWIFT_HOST_VARIANT_SDK STREQUAL WINDOWS)
    if(libkind STREQUAL SHARED)
      list(APPEND c_compile_flags -D_WINDLL)
    endif()
  endif()
  _add_variant_link_flags(
    SDK "${SWIFT_HOST_VARIANT_SDK}"
    ARCH "${SWIFT_HOST_VARIANT_ARCH}"
    BUILD_TYPE ${CMAKE_BUILD_TYPE}
    ENABLE_ASSERTIONS ${LLVM_ENABLE_ASSERTIONS}
    ANALYZE_CODE_COVERAGE ${SWIFT_ANALYZE_CODE_COVERAGE}
    ENABLE_LTO ${SWIFT_TOOLS_ENABLE_LTO}
    LTO_OBJECT_NAME "${target}-${SWIFT_HOST_VARIANT_SDK}-${SWIFT_HOST_VARIANT_ARCH}"
    RESULT_VAR_NAME link_flags
    LIBRARY_SEARCH_DIRECTORIES_VAR_NAME library_search_directories
      )

  # Set compilation and link flags.
  if(SWIFT_HOST_VARIANT_SDK STREQUAL WINDOWS)
    swift_windows_include_for_arch(${SWIFT_HOST_VARIANT_ARCH}
      ${SWIFT_HOST_VARIANT_ARCH}_INCLUDE)
    target_include_directories(${target} SYSTEM PRIVATE
      ${${SWIFT_HOST_VARIANT_ARCH}_INCLUDE})

    if(NOT ${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
      swift_windows_get_sdk_vfs_overlay(ASHLS_VFS_OVERLAY)
      target_compile_options(${target} PRIVATE
        "SHELL:-Xclang -ivfsoverlay -Xclang ${ASHLS_VFS_OVERLAY}")

      # MSVC doesn't support -Xclang. We don't need to manually specify
      # the dependent libraries as `cl` does so.
      target_compile_options(${target} PRIVATE
        "SHELL:-Xclang --dependent-lib=oldnames"
        # TODO(compnerd) handle /MT, /MTd
        "SHELL:-Xclang --dependent-lib=msvcrt$<$<CONFIG:Debug>:d>")
    endif()
  endif()

  target_compile_options(${target} PRIVATE
    ${c_compile_flags})
  target_link_options(${target} PRIVATE
    ${link_flags})
  if(${SWIFT_HOST_VARIANT_SDK} IN_LIST SWIFT_APPLE_PLATFORMS)
    target_link_options(${target} PRIVATE
      "LINKER:-compatibility_version,1")
    if(SWIFT_COMPILER_VERSION)
      target_link_options(${target} PRIVATE
        "LINKER:-current_version,${SWIFT_COMPILER_VERSION}")
    endif()
    # Include LLVM Bitcode slices for iOS, Watch OS, and Apple TV OS device libraries.
    if(SWIFT_EMBED_BITCODE_SECTION)
      if(${SWIFT_HOST_VARIANT_SDK} MATCHES "(I|TV|WATCH)OS")
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

  # Do not add code here.
endfunction()

# Add a new Swift host library.
#
# Usage:
#   add_swift_host_library(name
#     [SHARED]
#     [STATIC]
#     [LLVM_LINK_COMPONENTS comp1 ...]
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
# LLVM_LINK_COMPONENTS
#   LLVM components this library depends on.
#
# source1 ...
#   Sources to add into this library.
function(add_swift_host_library name)
  set(options
        SHARED
        STATIC)
  set(single_parameter_options)
  set(multiple_parameter_options
        LLVM_LINK_COMPONENTS)

  cmake_parse_arguments(ASHL
                        "${options}"
                        "${single_parameter_options}"
                        "${multiple_parameter_options}"
                        ${ARGN})
  set(ASHL_SOURCES ${ASHL_UNPARSED_ARGUMENTS})

  translate_flags(ASHL "${options}")

  if(NOT ASHL_SHARED AND NOT ASHL_STATIC)
    message(FATAL_ERROR "Either SHARED or STATIC must be specified")
  endif()

  _add_swift_host_library_single(
    ${name}
    ${ASHL_SHARED_keyword}
    ${ASHL_STATIC_keyword}
    ${ASHL_SOURCES}
    LLVM_LINK_COMPONENTS ${ASHL_LLVM_LINK_COMPONENTS}
    )

  add_dependencies(dev ${name})
  if(NOT LLVM_INSTALL_TOOLCHAIN_ONLY)
    swift_install_in_component(TARGETS ${name}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX} COMPONENT dev
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX} COMPONENT dev
      RUNTIME DESTINATION bin COMPONENT dev)
  endif()

  swift_is_installing_component(dev is_installing)
  if(NOT is_installing)
    set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS ${name})
  else()
    set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${name})
  endif()
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
function(_add_swift_host_executable_single name)
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

macro(add_swift_tool_subdirectory name)
  add_llvm_subdirectory(SWIFT TOOL ${name})
endmacro()

macro(add_swift_lib_subdirectory name)
  add_llvm_subdirectory(SWIFT LIB ${name})
endmacro()

function(add_swift_host_tool executable)
  set(options)
  set(single_parameter_options SWIFT_COMPONENT)
  set(multiple_parameter_options)

  cmake_parse_arguments(ASHT
    "${options}"
    "${single_parameter_options}"
    "${multiple_parameter_options}"
    ${ARGN})

  precondition(ASHT_SWIFT_COMPONENT
               MESSAGE "Swift Component is required to add a host tool")

  # Create the executable rule.
  _add_swift_host_executable_single(${executable}
    SDK ${SWIFT_HOST_VARIANT_SDK}
    ARCHITECTURE ${SWIFT_HOST_VARIANT_ARCH}
    ${ASHT_UNPARSED_ARGUMENTS})

  add_dependencies(${ASHT_SWIFT_COMPONENT} ${executable})
  swift_install_in_component(TARGETS ${executable}
                             RUNTIME
                               DESTINATION bin
                               COMPONENT ${ASHT_SWIFT_COMPONENT})

  swift_is_installing_component(${ASHT_SWIFT_COMPONENT} is_installing)

  if(NOT is_installing)
    set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS ${executable})
  else()
    set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${executable})
  endif()
endfunction()

# This declares a swift host tool that links with libfuzzer.
function(add_swift_fuzzer_host_tool executable)
  # First create our target. We do not actually parse the argument since we do
  # not care about the arguments, we just pass them all through to
  # add_swift_host_tool.
  add_swift_host_tool(${executable} ${ARGN})

  # Then make sure that we pass the -fsanitize=fuzzer flag both on the cflags
  # and cxx flags line.
  target_compile_options(${executable} PRIVATE "-fsanitize=fuzzer")
  target_link_libraries(${executable} PRIVATE "-fsanitize=fuzzer")
endfunction()

macro(add_swift_tool_symlink name dest component)
  add_llvm_tool_symlink(${name} ${dest} ALWAYS_GENERATE)
  llvm_install_symlink(${name} ${dest} ALWAYS_GENERATE COMPONENT ${component})
endmacro()
