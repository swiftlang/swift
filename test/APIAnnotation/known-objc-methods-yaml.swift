// ObjectiveC
// RUN: %swift_driver_plain -apinotes -yaml-to-binary %S/Inputs/ObjectiveC.yaml -o %t-ObjectiveC.apinotes
// RUN: %swift-ide-test -check-api-annotation ObjectiveC %t-ObjectiveC.apinotes

// Foundation
// RUN: %swift_driver_plain -apinotes -yaml-to-binary %S/Inputs/Foundation.yaml -o %t-Foundation.apinotes
// RUN: %swift-ide-test -check-api-annotation Foundation %t-Foundation.apinotes

// NotificationCenter
// RUN: %swift_driver_plain -apinotes -yaml-to-binary %S/Inputs/NotificationCenter.yaml -o %t-NotificationCenter.apinotes
// RUN: %swift-ide-test -check-api-annotation NotificationCenter %t-NotificationCenter.apinotes
