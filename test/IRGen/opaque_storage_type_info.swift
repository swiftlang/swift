// RUN: %target-swift-frontend -O -emit-ir -primary-file %s -module-name A
// REQUIRES: VENDOR=apple
import Foundation

public struct Fooz {
    var str = ""
    var dec = Decimal()
}
