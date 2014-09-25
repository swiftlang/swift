// ObjectiveC
// RUN: %clang_apinotes -yaml-to-binary %S/Inputs/ObjectiveC.yaml -o %t-ObjectiveC.apinotes
// RUN: %swift-ide-test -check-api-annotation ObjectiveC %t-ObjectiveC.apinotes

// Foundation
// RUN: %clang_apinotes -yaml-to-binary %S/Inputs/Foundation.yaml -o %t-Foundation.apinotes
// RUN: %swift-ide-test -check-api-annotation Foundation %t-Foundation.apinotes

// NotificationCenter
// RUN: %clang_apinotes -yaml-to-binary %S/Inputs/NotificationCenter.yaml -o %t-NotificationCenter.apinotes
// RUN: %swift-ide-test -check-api-annotation NotificationCenter %t-NotificationCenter.apinotes

// UIKit
// RUN: %clang_apinotes -yaml-to-binary %S/Inputs/UIKit.yaml -o %t-UIKit.apinotes
// RUN: %swift-ide-test -check-api-annotation UIKit %t-UIKit.apinotes
