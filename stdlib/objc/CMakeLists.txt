# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)

set(SWIFTOBJC_SOURCES
    ObjectiveC.swift)

# Definition of ObjCBool is platform-specific.
if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx" 
    OR
    (SWIFT_DEPLOYMENT_OS MATCHES "^iphone" 
     AND NOT (CMAKE_OSX_ARCHITECTURES MATCHES "64$")))
  # OS X or 32-bit iOS: Objective-C BOOL is signed char
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-BOOLchar.swift)
else()
  # Others: Objective-C BOOL is C99/C++ bool.
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-BOOLbool.swift)
endif()

add_swift_library(swiftObjectiveC INSTALL
  ${SWIFTOBJC_SOURCES}
  DEPENDS swift_stdlib_core)

if(SWIFT_MODULE_CACHE_PATH)
  set(module_cache "${SWIFT_MODULE_CACHE_PATH}")
elseif(APPLE)
  execute_process(COMMAND getconf DARWIN_USER_CACHE_DIR
    OUTPUT_VARIABLE user_cache_dir OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(user_cache_dir)
    set(module_cache "${user_cache_dir}/org.llvm.clang/ModuleCache")
  endif()
endif()
  
if(module_cache)
  add_custom_command(TARGET swiftObjectiveC PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${module_cache}"
    COMMENT "Clearing Clang module cache")
endif()

add_subdirectory(Foundation)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  add_subdirectory(AppKit)
else()
  add_subdirectory(UIKit)
endif()
