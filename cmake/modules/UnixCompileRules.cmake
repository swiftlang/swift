
#
# Assume a new enough ar to generate the index at construction time. This avoids
# having to invoke ranlib as a secondary command.
#

set(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_C_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_C_ARCHIVE_FINISH "")

set(CMAKE_CXX_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_CXX_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_CXX_ARCHIVE_FINISH "")

# When archiving LTO-based .o files with ar/ranlib/libtool on Darwin, the tools
# use libLTO.dylib to inspect the bitcode files. However, by default the
# "host" libLTO.dylib is loaded, which might be too old and not understand our
# just-built bitcode format. So let's instead ask ar/ranlib/libtool to use the
# just-built libLTO.dylib from the toolchain that we're using to build.
if(APPLE AND SWIFT_NATIVE_CLANG_TOOLS_PATH)
  set(liblto_path "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/../lib/libLTO.dylib")

  set(CMAKE_C_ARCHIVE_CREATE "${CMAKE_COMMAND} -E env LIBLTO_PATH=${liblto_path} <CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_C_ARCHIVE_APPEND "${CMAKE_COMMAND} -E env LIBLTO_PATH=${liblto_path} <CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_C_ARCHIVE_FINISH "")

  set(CMAKE_CXX_ARCHIVE_CREATE "${CMAKE_COMMAND} -E env LIBLTO_PATH=${liblto_path} <CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_CXX_ARCHIVE_APPEND "${CMAKE_COMMAND} -E env LIBLTO_PATH=${liblto_path} <CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_CXX_ARCHIVE_FINISH "")
endif()
