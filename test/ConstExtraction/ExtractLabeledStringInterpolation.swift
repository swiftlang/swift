// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -swift-version 5 -typecheck -emit-const-values-path %t/ExtractLabeledStringInterpolation.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractLabeledStringInterpolation.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

public enum PlaceholderType { case int, double, float, uint, object }

public struct StructArg {
    var name: String = "hello"
}

public class ClassArg {
    var name: String = "world"
    init() {}
}

public struct LabeledString: ExpressibleByStringInterpolation {
    public struct StringInterpolation: StringInterpolationProtocol {
        public var result: String = ""
        public init(literalCapacity: Int, interpolationCount: Int) {}
        public mutating func appendLiteral(_ literal: String) { result += literal }
        public mutating func appendInterpolation(placeholder: PlaceholderType) {}
        public mutating func appendInterpolation(placeholder: PlaceholderType, table: String) {}
        public mutating func appendInterpolation(structArg: StructArg) {}
        public mutating func appendInterpolation(classArg: ClassArg) {}
        public mutating func appendInterpolation(_ value: StructArg, label: PlaceholderType) {}
    }
    public var value: String
    public init(stringLiteral value: String) { self.value = value }
    public init(stringInterpolation: StringInterpolation) { self.value = stringInterpolation.result }
}

public struct WithPlaceholder: MyProto {
    var enumFormat: LabeledString = "\(placeholder: .int) books"
    var multiFormat: LabeledString = "\(placeholder: .int, table: "MyTable") books"
    var structFormat: LabeledString = "\(structArg: StructArg()) suffix"
    var classFormat: LabeledString = "\(classArg: ClassArg()) suffix"
    var mixedFormat: LabeledString = "\(StructArg(), label: .int) suffix"
}

// CHECK:      "typeName": "ExtractLabeledStringInterpolation.WithPlaceholder"

// -- Single-argument enum interpolation
// CHECK:      "label": "enumFormat"
// CHECK:      "valueKind": "InterpolatedStringLiteral"
// CHECK:      "segments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": ""
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "arguments": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "placeholder",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.PlaceholderType",
// CHECK-NEXT:         "valueKind": "Enum",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "name": "int"
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": " books"
// CHECK-NEXT:   }

// -- Multi-argument interpolation
// CHECK:      "label": "multiFormat"
// CHECK:      "valueKind": "InterpolatedStringLiteral"
// CHECK:      "segments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": ""
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "arguments": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "placeholder",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.PlaceholderType",
// CHECK-NEXT:         "valueKind": "Enum",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "name": "int"
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "table",
// CHECK-NEXT:         "type": "Swift.String",
// CHECK-NEXT:         "valueKind": "RawLiteral",
// CHECK-NEXT:         "value": "MyTable"
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": " books"
// CHECK-NEXT:   }

// -- Struct argument
// CHECK:      "label": "structFormat"
// CHECK:      "valueKind": "InterpolatedStringLiteral"
// CHECK:      "segments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": ""
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "arguments": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "structArg",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.StructArg",
// CHECK-NEXT:         "valueKind": "InitCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractLabeledStringInterpolation.StructArg",
// CHECK-NEXT:           "arguments": []
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": " suffix"
// CHECK-NEXT:   }

// -- Class argument
// CHECK:      "label": "classFormat"
// CHECK:      "valueKind": "InterpolatedStringLiteral"
// CHECK:      "segments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": ""
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "arguments": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "classArg",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.ClassArg",
// CHECK-NEXT:         "valueKind": "InitCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractLabeledStringInterpolation.ClassArg",
// CHECK-NEXT:           "arguments": []
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": " suffix"
// CHECK-NEXT:   }

// -- Mixed case: unlabeled first arg + labeled second arg → uses "arguments"
// CHECK:      "label": "mixedFormat"
// CHECK:      "valueKind": "InterpolatedStringLiteral"
// CHECK:      "segments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": ""
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "arguments": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.StructArg",
// CHECK-NEXT:         "valueKind": "InitCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractLabeledStringInterpolation.StructArg",
// CHECK-NEXT:           "arguments": []
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "label",
// CHECK-NEXT:         "type": "ExtractLabeledStringInterpolation.PlaceholderType",
// CHECK-NEXT:         "valueKind": "Enum",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "name": "int"
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": " suffix"
// CHECK-NEXT:   }
