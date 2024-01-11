// RUN: %empty-directory(%t)

// RUN: %target-build-swift -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -L%swift-lib-dir -lswiftGenericMetadataBuilder -enable-experimental-feature Extern %s -o %t/ExternalMetadataBuilderTest
// RUN: %target-codesign %t/ExternalMetadataBuilderTest
// RUN: %target-run %t/ExternalMetadataBuilderTest
// REQUIRES: executable_test
// REQUIRES: OS=macosx && CPU=arm64

import ExternalGenericMetadataBuilder
import Foundation
import StdlibUnittest

@_extern(c)
func swift_getRuntimeLibraryPath() -> UnsafePointer<CChar>?

let ExternalGenericMetadataBuilderTests = TestSuite("ExternalGenericMetadataBuilder")

public struct GenericStruct<T, U, V> {
  var t: T
  var u: U
  var v: V
  var str: String
}

public struct GenericField<T, U> {
  var field: GenericStruct<T, U, Double>
  var int: Int
}

ExternalGenericMetadataBuilderTests.test("JSON output") {
  let builder = swift_externalMetadataBuilder_create(1, "arm64")

  let inputJSON = """
  {
    "metadataNames": [
      {
        "name" : "\(_mangledTypeName(GenericField<Int8, Int16>.self)!)"
      },
      {
        "name" : "\(_mangledTypeName(Array<Array<Double>>.self)!)"
      }
    ]
  }
  """

  let readJSONErrorCStr = swift_externalMetadataBuilder_readNamesJSON(builder, inputJSON);
  let readJSONError = readJSONErrorCStr.map { String(cString: $0) }
  expectNil(readJSONError)

  let swiftCorePathCStr = swift_getRuntimeLibraryPath()
  let swiftCorePath = swiftCorePathCStr.map { String(cString: $0) }
  expectNotNil(swiftCorePath)

  // Add this executable as well, so we can test our own types.
  let executablePath = Bundle.main.executablePath

  for machoPath in [swiftCorePath, executablePath] {
    let url = URL(fileURLWithPath: machoPath!)
    let data = NSData(contentsOf: url)!

    let machHeader = data.bytes.assumingMemoryBound(to: mach_header.self)
    let addDylibErrorCStr =
      swift_externalMetadataBuilder_addDylib(builder,
                                             url.lastPathComponent,
                                             machHeader,
                                             UInt64(data.length));

    let addDylibError = addDylibErrorCStr.map { String(cString: $0) }
    expectNil(addDylibError)
  }

  let buildErrorCStr = swift_externalMetadataBuilder_buildMetadata(builder);
  let buildError = buildErrorCStr.map { String(cString: $0) }
  expectNil(buildError)

  let outputJSONCStr = swift_externalMetadataBuilder_getMetadataJSON(builder);
  let outputJSON = outputJSONCStr.map { String(cString: $0) }
  expectNotNil(outputJSON)

  let outputJSONObject = try! JSONSerialization.jsonObject(with: outputJSON!.data(using: .utf8)!)
  let expectedJSONObject = try! JSONSerialization.jsonObject(with: expectedJSON.data(using: .utf8)!)

  let outputJSONDictionary = outputJSONObject as? NSDictionary
  expectNotNil(outputJSONDictionary)
  let expectedJSONDictionary = expectedJSONObject as? NSDictionary
  expectNotNil(expectedJSONDictionary)

  // Don't use expectEqual, as it will print the strings on one line with \n
  // escapes, which is unreadable here.
  expectTrue(outputJSONDictionary!.isEqual(expectedJSONDictionary),
             "Output JSON does not match expected:\n\(outputJSON!)")

  swift_externalMetadataBuilder_destroy(builder)
}

runAllTests()

// Put the expected JSON at the end so it doesn't get in the way of the rest of
// the test code. Make it a computed property so we don't fall afoul of weird
// global initialization problems with top-level code.
var expectedJSON: String {
"""
{
    "version": 1,
    "platform": 1,
    "platformVersion": "1.0",
    "arch": "arm64",
    "installName": "/usr/lib/libswiftPrespecialized.dylib",
    "atoms": [
        {
            "name": "_$s27ExternalMetadataBuilderTest12GenericFieldVys4Int8Vs5Int16VG",
            "contentType": "constData",
            "contents": [
                "0000000000000000",
                {
                    "self": true,
                    "target": "___unnamed_atom_1",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "0002000000000000",
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMn",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$ss4Int8VN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$ss5Int16VN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "0000000020000000"
            ]
        },
        {
            "name": "_$s27ExternalMetadataBuilderTest13GenericStructVys4Int8Vs5Int16VSdG",
            "contentType": "constData",
            "contents": [
                "0000000000000000",
                {
                    "self": true,
                    "target": "___unnamed_atom_0",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "0002000000000000",
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest13GenericStructVMn",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$ss4Int8VN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$ss5Int16VN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sSdN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "00000000020000000800000010000000"
            ]
        },
        {
            "name": "___unnamed_atom_0",
            "contentType": "constData",
            "contents": [
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 1032,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 1376,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 1536,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 1768,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "__swift_pod_copy",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 2200,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 2420,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 2864,
                    "kind": "ptr64"
                },
                "2000000000000000200000000000000007000300FFFFFF7F"
            ]
        },
        {
            "name": "___unnamed_atom_1",
            "contentType": "constData",
            "contents": [
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 3520,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 3892,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 4052,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 4312,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "__swift_pod_copy",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 4796,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 5044,
                    "kind": "ptr64"
                },
                {
                    "target": "ExternalMetadataBuilderTest",
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa",
                    "addend": 5160,
                    "kind": "ptr64"
                },
                "2800000000000000280000000000000007000300FFFFFF7F"
            ]
        },
        {
            "name": "_$sSaySdG",
            "contentType": "constData",
            "contents": [
                "0000000000000000",
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sBbWV",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "0002000000000000",
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sSaMn",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sSdN",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "00000000000000000000000000000000"
            ]
        },
        {
            "name": "_$sSaySaySdGG",
            "contentType": "constData",
            "contents": [
                "0000000000000000",
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sBbWV",
                    "addend": 0,
                    "kind": "ptr64"
                },
                "0002000000000000",
                {
                    "target": "/usr/lib/swift/libswiftCore.dylib",
                    "target": "_$sSaMn",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "_$sSaySdG",
                    "addend": 16,
                    "kind": "ptr64"
                },
                "00000000000000000000000000000000"
            ]
        },
        {
            "name": "__swift_prespecializedMetadataMap",
            "contentType": "constData",
            "contents": [
                "0500000000000000",
                {
                    "self": true,
                    "target": "__cstring_$s27ExternalMetadataBuilderTest13GenericStructVys4Int8Vs5Int16VSdG",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "_$s27ExternalMetadataBuilderTest13GenericStructVys4Int8Vs5Int16VSdG",
                    "addend": 16,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "__cstring_$sSaySdG",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "_$sSaySdG",
                    "addend": 16,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "__cstring_$s27ExternalMetadataBuilderTest12GenericFieldVys4Int8Vs5Int16VG",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "_$s27ExternalMetadataBuilderTest12GenericFieldVys4Int8Vs5Int16VG",
                    "addend": 16,
                    "kind": "ptr64"
                },
                "00000000000000000000000000000000",
                {
                    "self": true,
                    "target": "__cstring_$sSaySaySdGG",
                    "addend": 0,
                    "kind": "ptr64"
                },
                {
                    "self": true,
                    "target": "_$sSaySaySdGG",
                    "addend": 16,
                    "kind": "ptr64"
                }
            ]
        },
        {
            "name": "__cstring_$s27ExternalMetadataBuilderTest12GenericFieldVys4Int8Vs5Int16VG",
            "contentType": "constData",
            "contents": [
                "2473323745787465726E616C4D657461646174614275696C64657254657374313247656E657269634669656C6456797334496E7438567335496E743136564700"
            ]
        },
        {
            "name": "__cstring_$s27ExternalMetadataBuilderTest13GenericStructVys4Int8Vs5Int16VSdG",
            "contentType": "constData",
            "contents": [
                "2473323745787465726E616C4D657461646174614275696C64657254657374313347656E6572696353747275637456797334496E7438567335496E7431365653644700"
            ]
        },
        {
            "name": "__cstring_$sSaySaySdGG",
            "contentType": "constData",
            "contents": [
                "24735361795361795364474700"
            ]
        },
        {
            "name": "__cstring_$sSaySdG",
            "contentType": "constData",
            "contents": [
                "247353617953644700"
            ]
        },
        {
            "name": "__swift_prespecializationsData",
            "contentType": "constData",
            "contents": [
                "0100000001000000",
                {
                    "self": true,
                    "target": "__swift_prespecializedMetadataMap",
                    "addend": 0,
                    "kind": "ptr64"
                }
            ]
        }
    ],
    "dylibs": [
        {
            "installName": "/usr/lib/swift/libswiftCore.dylib",
            "exports": [
                {
                    "name": "_$sSaMn"
                },
                {
                    "name": "_$ss4Int8VN"
                },
                {
                    "name": "_$sSdN"
                },
                {
                    "name": "_$sBbWV"
                },
                {
                    "name": "__swift_pod_copy"
                },
                {
                    "name": "_$ss5Int16VN"
                }
            ]
        },
        {
            "installName": "ExternalMetadataBuilderTest",
            "exports": [
                {
                    "name": "_$s27ExternalMetadataBuilderTest12GenericFieldVMa"
                },
                {
                    "name": "_$s27ExternalMetadataBuilderTest13GenericStructVMn"
                },
                {
                    "name": "_$s27ExternalMetadataBuilderTest12GenericFieldVMn"
                }
            ]
        }
    ]
}
"""
}
