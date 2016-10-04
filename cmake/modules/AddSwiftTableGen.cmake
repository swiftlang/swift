include(TableGen)

# This needs to be a macro since tablegen (which is a function) needs to set
# variables in its parent scope.
macro(swift_tablegen)
  tablegen(SWIFT ${ARGN})
endmacro()

# This needs to be a macro since add_public_tablegen_target (which is a
# function) needs to set variables in its parent scope.
macro(swift_add_public_tablegen_target target)
  # FIXME: LLVM's add_public_tablegen_target should take a DEPENDS argument,
  #        allowing dependencies to be set without modifying
  #        LLVM_COMMON_DEPENDS.
  set(_original_llvm_common_depends ${LLVM_COMMON_DEPENDS})
  set(LLVM_COMMON_DEPENDS ${SWIFT_COMMON_DEPENDS})
  add_public_tablegen_target(${target})
  set(LLVM_COMMON_DEPENDS ${_original_llvm_common_depends})
endmacro()
