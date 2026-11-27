# The parts of swift-system that the experimental io_uring backend of
# `PlatformExecutors` uses, built from the swift-system checkout as private
# modules, like `_PlatformExecutorsCollections`:
#
# - `_PlatformExecutorsSystem`, from the `SystemPackage` target.
# - `_PlatformExecutorsSystemSockets`, from the `SystemSockets` target.
# - `_PlatformExecutorsSystemCShims`, the C module `CSystem`.
#
# The sources keep importing `SystemPackage`, `SystemSockets` and `CSystem`,
# and are compiled with `-module-alias` to the private names, so that the
# toolchain doesn't ship modules that clash with the swift-system package. Only
# `PlatformExecutors` may import them, with `internal import`.
#
# swift-system has no CMake build for `SystemSockets`, and its CMake build of
# `SystemPackage` can't name the module, so the sources of the checkout are
# listed here. That can move to the CMakeLists.txt files of swift-system once
# they can build these modules for the toolchain, like swift-collections does
# for `_PlatformExecutorsCollections`.
set(_system_source_dir "${SwiftPlatformExecutors_SWIFT_SYSTEM_SOURCE_DIR}/Sources")

# The C module, with the headers and the module map of `CSystem` under its
# private name. The module map has the private name, so the sources are
# compiled with `-module-alias CSystem=_PlatformExecutorsSystemCShims`.
set(_system_c_include_dir "${_system_source_dir}/CSystem/include")
file(GLOB _system_c_headers CONFIGURE_DEPENDS "${_system_c_include_dir}/*.h")
set(_system_c_module_dir "${CMAKE_CURRENT_BINARY_DIR}/_PlatformExecutorsSystemCShims")
foreach(header ${_system_c_headers})
  get_filename_component(header_name "${header}" NAME)
  configure_file("${header}" "${_system_c_module_dir}/${header_name}" COPYONLY)
endforeach()
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${_system_c_include_dir}/module.modulemap")
file(READ "${_system_c_include_dir}/module.modulemap" _system_c_module_map_contents)
string(REGEX REPLACE "module[ \t]+CSystem[ \t]*{" "module _PlatformExecutorsSystemCShims {"
  _system_c_module_map_contents "${_system_c_module_map_contents}")
file(CONFIGURE OUTPUT "${_system_c_module_dir}/module.modulemap"
  CONTENT "${_system_c_module_map_contents}")
# For `PlatformExecutors`, which loads the modules that import it.
set(SwiftPlatformExecutors_SYSTEM_C_MODULE_MAP "${_system_c_module_dir}/module.modulemap" PARENT_SCOPE)

add_library(swift_PlatformExecutorsSystemCShims OBJECT
  "${_system_source_dir}/CSystem/shims.c")
target_include_directories(swift_PlatformExecutorsSystemCShims PRIVATE
  "${_system_c_include_dir}")

# The `System X.Y.Z` availability macros of swift-system. They only matter on
# the Apple platforms, so define each version that the Package.swift of
# swift-system lists with the availability of the sources there.
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${SwiftPlatformExecutors_SWIFT_SYSTEM_SOURCE_DIR}/Package.swift")
file(STRINGS "${SwiftPlatformExecutors_SWIFT_SYSTEM_SOURCE_DIR}/Package.swift"
  _system_availability REGEX "Available\\(\"[0-9.]+\"")
set(_system_compile_options)
foreach(line ${_system_availability})
  string(REGEX MATCH "Available\\(\"([0-9.]+)\"" _ "${line}")
  list(APPEND _system_compile_options
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -define-availability -Xfrontend \"System ${CMAKE_MATCH_1}:macOS 10.10, iOS 8.0, watchOS 2.0, tvOS 9.0, visionOS 1.0\">")
endforeach()

# The Swift settings of the Package.swift of swift-system.
list(APPEND _system_compile_options
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-package-name swift-system>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D SYSTEM_PACKAGE>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias CSystem=_PlatformExecutorsSystemCShims>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${_system_c_module_dir}/module.modulemap>")

file(GLOB_RECURSE _system_sources CONFIGURE_DEPENDS
  "${_system_source_dir}/System/*.swift")
add_library(swift_PlatformExecutorsSystem ${_system_sources})
set_target_properties(swift_PlatformExecutorsSystem PROPERTIES
  Swift_MODULE_NAME _PlatformExecutorsSystem
  Swift_LANGUAGE_VERSION 6)
target_compile_options(swift_PlatformExecutorsSystem PRIVATE
  ${_system_compile_options}
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -allowable-client -Xfrontend PlatformExecutors>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -allowable-client -Xfrontend _PlatformExecutorsSystemSockets>")
# `IORing` uses `Synchronization`.
target_link_libraries(swift_PlatformExecutorsSystem PRIVATE
  swift_PlatformExecutorsSystemCShims
  swiftSynchronization
  swiftCore
  swiftGlibc
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

file(GLOB_RECURSE _system_sockets_sources CONFIGURE_DEPENDS
  "${_system_source_dir}/SystemSockets/*.swift")
add_library(swift_PlatformExecutorsSystemSockets ${_system_sockets_sources})
set_target_properties(swift_PlatformExecutorsSystemSockets PROPERTIES
  Swift_MODULE_NAME _PlatformExecutorsSystemSockets
  Swift_LANGUAGE_VERSION 6)
target_compile_options(swift_PlatformExecutorsSystemSockets PRIVATE
  ${_system_compile_options}
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias SystemPackage=_PlatformExecutorsSystem>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -allowable-client -Xfrontend PlatformExecutors>")
target_link_libraries(swift_PlatformExecutorsSystemSockets PRIVATE
  swift_PlatformExecutorsSystem
  swiftCore
  swiftGlibc
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swift_PlatformExecutorsSystem swift_PlatformExecutorsSystemSockets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

if(BUILD_SHARED_LIBS)
  set(_system_c_install_dir "${CMAKE_INSTALL_LIBDIR}/swift/_PlatformExecutorsSystemCShims")
else()
  set(_system_c_install_dir "${CMAKE_INSTALL_LIBDIR}/swift_static/_PlatformExecutorsSystemCShims")
endif()
install(FILES
  ${_system_c_headers}
  "${_system_c_module_dir}/module.modulemap"
  DESTINATION "${_system_c_install_dir}"
  COMPONENT ${PROJECT_NAME}_development)

# swift-system isn't built with library evolution, and only `PlatformExecutors`
# imports these modules, with `internal import`, so build them without it, like
# `_PlatformExecutorsCollections`. This only changes the variable in this
# directory, so that `emit_swift_interface` emits just the binary modules.
set(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION NO)
emit_swift_interface(swift_PlatformExecutorsSystem)
install_swift_interface(swift_PlatformExecutorsSystem)
emit_swift_interface(swift_PlatformExecutorsSystemSockets)
install_swift_interface(swift_PlatformExecutorsSystemSockets)

embed_manifest(swift_PlatformExecutorsSystem)
embed_manifest(swift_PlatformExecutorsSystemSockets)
