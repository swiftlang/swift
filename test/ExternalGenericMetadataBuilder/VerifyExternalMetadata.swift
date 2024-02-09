// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -enable-experimental-feature Extern %s -o %t/VerifyExternalMetadata
// RUN: %target-codesign %t/VerifyExternalMetadata
//
// RUN: %host-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -L%swift-lib-dir -lswiftGenericMetadataBuilder -Xlinker -rpath -Xlinker %swift-lib-dir -enable-experimental-feature Extern %S/Inputs/buildMetadataJSON.swift -o %t/buildMetadataJSON
// no: %target-build-swift -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -L%swift-lib-dir -lswiftGenericMetadataBuilder -enable-experimental-feature Extern %S/Inputs/buildMetadataJSON.swift -o %t/buildMetadataJSON
// RUN: %target-codesign %t/buildMetadataJSON
//
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %S/Inputs/json2c.swift -o %t/json2c
// RUN: %target-codesign %t/json2c
//
// RUN: %target-run %t/VerifyExternalMetadata getJSON > %t/names.json
// RUN: %target-run %t/buildMetadataJSON %target-arch %t/VerifyExternalMetadata %stdlib_dir/libswiftCore.dylib < %t/names.json > %t/libswiftPrespecialized.json
// RUN: %target-run %t/json2c %t/libswiftPrespecialized.json > %t/libswiftPrespecialized.c
// RUN: %clang -isysroot %sdk -target %target-triple -bundle %t/libswiftPrespecialized.c -L%stdlib_dir -lswiftCore -bundle_loader %t/VerifyExternalMetadata -o %t/libswiftPrespecialized.bundle
//
//
// RUN: env SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_LOGGING=y SWIFT_DEBUG_LIB_PRESPECIALIZED_PATH=%t/libswiftPrespecialized.bundle %target-run %t/VerifyExternalMetadata

// REQUIRES: executable_test
// REQUIRES: OS=macosx && CPU=arm64

import ExternalGenericMetadataBuilder
import Foundation
import StdlibUnittest

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

let args = CommandLine.arguments

if args.count > 1 && args[1] == "getJSON" {
  let types: [Any.Type] = [
    Array<Double>.self,
    GenericField<GenericField<Int8, Int16>,
                 Array<GenericStruct<Double, String, Float>>>.self,
    Array<Array<Array<Array<Array<Array<Array<Double>>>>>>>.self,
  ]
  let typeNames = types.map { _mangledTypeName($0)! }
  let jsonNames = typeNames.map { [ "name": $0 ] }
  let jsonStructure = [ "metadataNames": jsonNames ]
  let jsonData = try! JSONSerialization.data(withJSONObject: jsonStructure)
  FileHandle.standardOutput.write(jsonData)
  exit(0)
}

var validated: CUnsignedInt = 0
var failed: CUnsignedInt = 0

@_extern(c)
func _swift_validatePrespecializedMetadata(_ outValidated: UnsafeMutablePointer<CUnsignedInt>, _ outFailed: UnsafeMutablePointer<CUnsignedInt>)
_swift_validatePrespecializedMetadata(&validated, &failed)
if failed > 0 {
  fatalError("\(failed) prespecialized metadata records failed validation")
}
if validated == 0 {
  fatalError("zero prespecialized metadata records validated, there should be some")
}

print("Verified \(validated) records, success!")
