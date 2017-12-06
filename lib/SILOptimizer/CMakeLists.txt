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
  LINK_LIBRARIES swiftSIL)
