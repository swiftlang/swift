# The containers of swift-collections that `PlatformExecutors` uses, such as
# `UniqueArray` and `UniqueDeque`, built as one private module.
#
# The module has its own name, so its symbols can't clash with a copy of
# swift-collections in the same process, and only `PlatformExecutors` may
# import it. `PlatformExecutors` imports it with `internal import`, so it
# doesn't appear in the interface of `PlatformExecutors`. Foundation does the
# same with `_FoundationCollections`.
#
# The sources are built in place from the swift-collections checkout, with the
# CMakeLists.txt files of its modules. When swift-collections is built as a
# single module, those only add their sources to the `COLLECTIONS_MODULE_NAME`
# target, so the source lists stay in swift-collections.
add_library(swift_PlatformExecutorsCollections)
set_target_properties(swift_PlatformExecutorsCollections PROPERTIES
  Swift_MODULE_NAME _PlatformExecutorsCollections
  Swift_LANGUAGE_VERSION 6)

set(COLLECTIONS_SINGLE_MODULE YES)
set(COLLECTIONS_MODULE_NAME swift_PlatformExecutorsCollections)
foreach(module InternalCollectionsUtilities SpanPreview BasicContainers DequeModule)
  add_subdirectory("${SwiftPlatformExecutors_SWIFT_COLLECTIONS_SOURCE_DIR}/Sources/${module}" "${module}")
endforeach()

# The sources are outside of this project, so the paths of their object files
# contain the full paths of the sources (`C_/.../swift-collections/Sources/...`).
# On Windows, CMake only shortens object paths longer than 250 characters, but
# object paths of 248 characters (`MAX_PATH - 12`) already fail to link, so
# shorten all of them below that.
set(CMAKE_OBJECT_PATH_MAX 240)

# The Swift settings of the Package.swift of swift-collections, with the
# `UnstableHashedContainers` trait enabled for `RigidDictionary` and
# `UniqueDictionary`.
target_compile_options(swift_PlatformExecutorsCollections PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-package-name swift-collections>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D COLLECTIONS_SINGLE_MODULE>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D UnstableHashedContainers>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BuiltinModule>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature InoutLifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypesWithDefaults>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -allowable-client -Xfrontend PlatformExecutors>")
target_link_libraries(swift_PlatformExecutorsCollections PRIVATE
  swiftCore
  $<$<PLATFORM_ID:Android>:swiftAndroid>
  $<$<PLATFORM_ID:Linux>:swiftGlibc>
  $<$<PLATFORM_ID:Windows>:swiftCRT>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swift_PlatformExecutorsCollections
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

# swift-collections isn't built with library evolution, and its sources don't
# support it. The module doesn't need a stable ABI, because only
# `PlatformExecutors` imports it, with `internal import`, so build it without
# library evolution, like Foundation builds `_FoundationCollections`. This only
# changes the variable in this directory, so that `emit_swift_interface` emits
# just the binary module. The clients of the builds without library evolution
# load the module, so it's installed like the other modules.
set(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION NO)
emit_swift_interface(swift_PlatformExecutorsCollections)
install_swift_interface(swift_PlatformExecutorsCollections)

embed_manifest(swift_PlatformExecutorsCollections)
