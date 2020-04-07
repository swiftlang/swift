# Find libc++

find_library(LibCXX_LIBRARY NAMES c++)
find_library(LibCXXABI_LIBRARY NAMES c++abi)

find_path(LibCXX_PREFIX c++/v1/algorithm PATHS ${LibCXX_LIB_PATH}/../include)
if (LibCXX_PREFIX)
  set(LibCXX_INCLUDE_DIR ${LibCXX_PREFIX}/c++/v1/)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibCXX DEFAULT_MSG
  LibCXX_LIBRARY
  LibCXXABI_LIBRARY
  LibCXX_INCLUDE_DIR)
