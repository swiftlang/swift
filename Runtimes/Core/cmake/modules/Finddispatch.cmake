#[=======================================================================[.rst:
Finddispatch
------------

Find libdispatch on macOS and Linux while deferring to dispatchConcig.cmake when
available or requested.

Components
^^^^^^^^^^

This module supports the optional component `Swift`, for use with the
`COMPONENTS` argument of the :command:`find_package` command. This component
defines the associated ``swiftDispatch`` IMPORT target.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``dispatch``

 ``swiftDispatch``

Result Variables
^^^^^^^^^^^^^^^^

The module may set the following variables if `dispatch_DIR` is not set.

 ``dispatch_FOUND``
   true if dispatch headers and libraries were found

 ``dispatch_INCLUDE_DIR``
   the directory containing the libdispatch headers

 ``dispatch_LIBRARIES``
   the libraries to be linked


#]=======================================================================]

# If the dispatch_DIR is specified, look there instead. The cmake-generated
# config file is more accurate, but requires that the SDK has one available.
if(dispatch_DIR)
  if(dispatch_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(dispatch_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(dispatch NO_MODULE ${args})
  return()
endif()

if(APPLE)
  find_path(dispatch_INCLUDE_DIR "dispatch/dispatch.h")
  find_library(dispatch_LIBRARY NAMES "libdispatch.tbd"
    PATH "usr/lib/system"
    PATH_SUFFIXES system)

  add_library(dispatch SHARED IMPORTED GLOBAL)
  set_target_properties(dispatch PROPERTIES
    IMPORTED_IMPLIB "${dispatch_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${dispatch_INCLUDE_DIR}")
else()
  # TODO: Implement this for Linux and Windows
message(FATAL_ERROR "Not implemented for platform")
endif()
