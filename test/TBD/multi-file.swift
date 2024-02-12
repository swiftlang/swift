// RUN: %empty-directory(%t)

// -Onone, non-resilient

// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -wmo -Xfrontend -validate-tbd-against-ir=all
// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -Xfrontend -validate-tbd-against-ir=all

// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-wmo.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -wmo
// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-incremental.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift
// RUN: diff %t/TBD-wmo.tbd %t/TBD-incremental.tbd

// -O, non-resilient

// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -wmo -O -Xfrontend -validate-tbd-against-ir=all
// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -O -Xfrontend -validate-tbd-against-ir=all

// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-wmo.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -wmo -O
// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-incremental.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -O
// RUN: diff %t/TBD-wmo.tbd %t/TBD-incremental.tbd

// -Onone, resilient

// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -wmo -enable-library-evolution -Xfrontend -validate-tbd-against-ir=all
// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -enable-library-evolution -Xfrontend -validate-tbd-against-ir=all

// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-wmo.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -wmo -enable-library-evolution
// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-incremental.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -enable-library-evolution
// RUN: diff %t/TBD-wmo.tbd %t/TBD-incremental.tbd

// -O, resilient

// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -wmo -O -enable-library-evolution -Xfrontend -validate-tbd-against-ir=all
// RUN: %target-build-swift -swift-version 4 -module-name multifile -emit-library -o %t/JustForTBDValidation %s %S/Inputs/multi-file2.swift -O -enable-library-evolution -Xfrontend -validate-tbd-against-ir=all

// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-wmo.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -wmo -O -enable-library-evolution
// RUN: %target-build-swift -swift-version 4 -module-name multifile -Xfrontend -tbd-install_name -Xfrontend multifile -emit-tbd-path %t/TBD-incremental.tbd -emit-module-path %t/multifile.swiftmodule %s %S/Inputs/multi-file2.swift -O -enable-library-evolution
// RUN: diff %t/TBD-wmo.tbd %t/TBD-incremental.tbd

// REQUIRES: objc_interop

public func function() {}

public class Class {
    public var property: Int

    public var propertyWithInit: Int = 0

    public init() {
        property = 0
    }

    public static func staticFunc(default_: Int = 0) {}
}
