# If we're building overlays, make sure we mention the ones we need.
# Otherwise, assume they already exist.
set(darwin)
set(wincrt_sdk)
set(glibc)
set(musl)

if(SWIFT_BUILD_SDK_OVERLAY)
  set(darwin Darwin)
  set(wincrt_sdk CRT WinSDK)
  set(glibc Glibc)
  set(musl Musl)
endif()

if((SWIFT_BUILD_CLANG_OVERLAYS
    OR SWIFT_BUILD_TEST_SUPPORT_MODULES)
   AND (NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT
        OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT))
  # We need to make this dependency explicit because this library
  # imports Darwin and Darwin re-exports _Builtin_float,
  # but in most configurations we pull Darwin from the SDK,
  # meaning we specify no Darwin dependency in the build system
  list(APPEND darwin _Builtin_float)
endif()

# Similarly, we only want the Runtime dependency if we're building
# with the stdlib.
set(runtime)
if(SWIFT_BUILD_STDLIB AND SWIFT_ENABLE_RUNTIME_MODULE)
  set(runtime Runtime)
endif()

if(NOT SWIFT_BUILD_STDLIB)
  set(BUILD_STANDALONE TRUE)
endif()

set(BACKTRACING_COMPILE_FLAGS
  "-cxx-interoperability-mode=default"
  "-I${SWIFT_STDLIB_SOURCE_DIR}/public/RuntimeModule/modules"
  "-Xcc;-I${SWIFT_SOURCE_DIR}/include"
  "-Xcc;-I${CMAKE_BINARY_DIR}/include"
  "-disable-upcoming-feature;MemberImportVisibility")

set(BACKTRACING_SOURCES
  main.swift
  AnsiColor.swift
  JSON.swift
  TargetMacOS.swift
  TargetLinux.swift
  Themes.swift
  Utils.swift
  )

# We have to build with a deployment target of at least 10.15, otherwise
# the tests will all fail because dyld will get confused at the use of
# @rpath (from magic-symbols-for-install-name.c) and the `some Sequence<Frame>`
# in Backtrace won't be accessible.
if(SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX VERSION_LESS "10.15")
  set(osx_deployment_target "10.15")
else()
  set(osx_deployment_target "${SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX}")
endif()

add_swift_target_executable(swift-backtrace BUILD_WITH_LIBEXEC
  ${BACKTRACING_SOURCES}

  SWIFT_MODULE_DEPENDS         ${runtime}

  SWIFT_MODULE_DEPENDS_OSX          ${darwin}
  SWIFT_MODULE_DEPENDS_WINDOWS      ${wincrt_sdk}
  SWIFT_MODULE_DEPENDS_LINUX        ${glibc}
  SWIFT_MODULE_DEPENDS_LINUX_STATIC ${musl}

  DEPLOYMENT_VERSION_OSX ${osx_deployment_target}

  INSTALL_IN_COMPONENT libexec
  COMPILE_FLAGS
    ${BACKTRACING_COMPILE_FLAGS}
    -parse-as-library

  TARGET_SDKS OSX LINUX)

set(static_target_sdks)
if(SWIFT_BUILD_STATIC_STDLIB)
  list(APPEND static_target_sdks "LINUX")
endif()
if("LINUX_STATIC" IN_LIST SWIFT_SDKS)
  list(APPEND static_target_sdks "LINUX_STATIC")
endif()
if(static_target_sdks)
  add_swift_target_executable(swift-backtrace-static BUILD_WITH_LIBEXEC
    PREFER_STATIC

    ${BACKTRACING_SOURCES}

    SWIFT_MODULE_DEPENDS         ${runtime}

    SWIFT_MODULE_DEPENDS_OSX          ${darwin}
    SWIFT_MODULE_DEPENDS_WINDOWS      ${wincrt_sdk}
    SWIFT_MODULE_DEPENDS_LINUX        ${glibc}
    SWIFT_MODULE_DEPENDS_LINUX_STATIC ${musl}

    DEPLOYMENT_VERSION_OSX ${osx_deployment_target}

    INSTALL_IN_COMPONENT libexec
    COMPILE_FLAGS
      ${BACKTRACING_COMPILE_FLAGS}
      -parse-as-library

    TARGET_SDKS ${static_target_sdks})
endif()
