# RUN: %swift_driver_plain -apinotes -yaml-to-binary -o %t.apinotesc %S/Inputs/roundtrip.apinotes
# RUN: %swift_driver_plain -apinotes -binary-to-yaml -o %t.apinotes %t.apinotesc

# Note: We don't diff the results because they will change as Clang's
# API notes support evolves, and we don't want to tie that closely to
# a specific Clang.

