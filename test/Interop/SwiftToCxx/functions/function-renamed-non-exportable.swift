// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -emit-module -emit-clang-header-path %t/functions.h -enable-library-evolution -experimental-skip-non-exportable-decls
// RUN: %FileCheck %s < %t/functions.h

// Verify that a deprecated method with @available(*, renamed:) pointing to
// a non-exportable target does not crash the Cxx header printer.
// The printer should fall back to using the raw rename string from the
// attribute when the renamed decl is not includable.
// rdar://176190799

public enum ExportFormat {
    case string
    case file
}

public struct MyStruct {
    public init() {}

    // The renamed target `export(to:addSourceFileComment:)` throws, making it
    // non-exportable with -experimental-skip-non-exportable-decls. The printer
    // should not crash and should emit the raw rename string.
    //
    // CHECK: SWIFT_DEPRECATED_MSG("", "export(to:addSourceFileComment:)")
    @available(*, deprecated, renamed: "export(to:addSourceFileComment:)")
    public func exportToString(addSourceFileComment: Bool = true) -> String? {
        return nil
    }

    public func export(
        to format: ExportFormat,
        addSourceFileComment: Bool = true
    ) throws -> String {
        return ""
    }
}
