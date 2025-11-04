include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

add_library(swiftClientRetainRelease
  STATIC  RetainRelease.s)

# FIXME: swift/ABI/System.h from the compiler
#        headers, which imports System.h from the shims
#        We should split out the parts that are needed by the runtime
#        to avoid pulling in headers from the compiler.
target_include_directories(swiftClientRetainRelease PRIVATE
  "${SwiftToolchain_SWIFTC_SOURCE_DIR}/include"
  "${CMAKE_SOURCE_DIR}/../Core/SwiftShims")

target_compile_options(swiftClientRetainRelease PRIVATE
  $<$<BOOL:${SwiftRetainReleaseClient_ENABLE_OBJC_INTEROP}>:-DSWIFT_OBJC_INTEROP=1>)

install(TARGETS swiftClientRetainRelease
  EXPORT SwiftClientRetainReleaseTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

# Inter-project install info
export(EXPORT SwiftClientRetainReleaseTargets
  FILE "cmake/SwiftClientRetainRelease/SwiftClientRetainReleaseTargets.cmake")
install(EXPORT SwiftClientRetainReleaseTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease"
  FILE "SwiftClientRetainReleaseTargets.cmake"
  COMPONENT SwiftClientRetainReleaseCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftClientRetainReleaseConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftClientRetainReleaseConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftClientRetainReleaseConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftClientRetainReleaseConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftClientRetainReleaseConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftClientRetainRelease.cmake" OPTIONAL)
