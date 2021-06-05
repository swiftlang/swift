// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -emit-module-path %t/PrunedBaseClass.swiftmodule -c -o %t/PrunedBaseClass.o

import PrunedBaseClass

public class PrunedSub: PrunedBase {
    func exerciseNonoverriddenMethod() {
        self.nonoverridden()
        super.nonoverridden()
    }
}

PrunedSub().exerciseNonoverriddenMethod()
