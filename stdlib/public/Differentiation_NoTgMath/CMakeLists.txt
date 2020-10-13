set(SOURCES_FOLDER ../Differentiation)

add_swift_target_library(swift_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${SOURCES_FOLDER}/Differentiable.swift
  ${SOURCES_FOLDER}/DifferentialOperators.swift
  ${SOURCES_FOLDER}/DifferentiationUtilities.swift
  ${SOURCES_FOLDER}/AnyDifferentiable.swift
  ${SOURCES_FOLDER}/ArrayDifferentiation.swift
  ${SOURCES_FOLDER}/OptionalDifferentiation.swift

  GYB_SOURCES
    ${SOURCES_FOLDER}/FloatingPointDifferentiation.swift.gyb
    ${SOURCES_FOLDER}/SIMDDifferentiation.swift.gyb

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  DARWIN_INSTALL_NAME_DIR "@rpath"
  INSTALL_IN_COMPONENT stdlib)
