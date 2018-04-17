if(SWIFT_ENABLE_TENSORFLOW)
  find_package(TensorFlow REQUIRED)
  include_directories(BEFORE "${TF_INCLUDE_DIR}")
endif()
add_subdirectory(ARC)
add_subdirectory(Analysis)
add_subdirectory(IPO)
add_subdirectory(LoopTransforms)
add_subdirectory(Mandatory)
add_subdirectory(PassManager)
add_subdirectory(SILCombiner)
add_subdirectory(Transforms)
add_subdirectory(UtilityPasses)
add_subdirectory(Utils)
add_swift_library(swiftSILOptimizer STATIC
  ${ARC_SOURCES}
  ${ANALYSIS_SOURCES}
  ${SILCOMBINER_SOURCES}
  ${UTILITYPASSES_SOURCES}
  ${UTILS_SOURCES}
  ${PASSMANAGER_SOURCES}
  ${LOOPTRANSFORMS_SOURCES}
  ${MANDATORY_SOURCES}
  ${TRANSFORMS_SOURCES}
  ${IPO_SOURCES}
  LINK_LIBRARIES swiftSIL
  INTERFACE_LINK_LIBRARIES "${TF_LIBRARIES}")
