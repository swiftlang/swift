# Find libicu's libraries

include(FindPackageHandleStandardArgs)

find_package(PkgConfig)

set(ICU_REQUIRED)
foreach(MODULE ${ICU_FIND_COMPONENTS})
  string(TOUPPER "${MODULE}" MODULE)
  string(TOLOWER "${MODULE}" module)
  list(APPEND ICU_REQUIRED 
    ICU_${MODULE}_INCLUDE_DIR ICU_${MODULE}_LIBRARIES)

  pkg_check_modules(PC_ICU_${MODULE} QUIET icu-${module})
  if(${PC_ICU_${MODULE}_FOUND})
    set(ICU_${MODULE}_DEFINITIONS ${PC_ICU_${MODULE}_CFLAGS_OTHER})

    find_path(ICU_${MODULE}_INCLUDE_DIR unicode
      HINTS ${PC_ICU_${MODULE}_INCLUDEDIR} ${PC_ICU_${MODULE}_INCLUDE_DIRS})
    set(ICU_${MODULE}_INCLUDE_DIR ${ICU_${MODULE}_INCLUDE_DIR})

    find_library(ICU_${MODULE}_LIBRARY NAMES icu${module}
      HINTS ${PC_ICU_${MODULE}_LIBDIR} ${PC_ICU_${MODULE}_LIBRARY_DIRS})
    set(ICU_${MODULE}_LIBRARIES ${ICU_${MODULE}_LIBRARY})
  endif()
endforeach()

find_package_handle_standard_args(ICU DEFAULT_MSG ${ICU_REQUIRED})
mark_as_advanced(${ICU_REQUIRED})
