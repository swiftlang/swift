add_gyb_target(generated_sourcekit_uids
  ProtocolUIDs.def.gyb
  DEPENDS
    "${SWIFT_SOURCE_DIR}/utils/gyb_sourcekit_support/__init__.py"
    "${SWIFT_SOURCE_DIR}/utils/gyb_sourcekit_support/UIDs.py")
