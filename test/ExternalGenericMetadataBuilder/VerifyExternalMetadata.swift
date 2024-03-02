// RUN: %empty-directory(%t)
//
// RUN: %host-build-swift %S/Inputs/testMetadataLibrary.swift -emit-library -emit-module -o %t/libtestMetadataLibrary.dylib
// RUN: %target-codesign %t/libtestMetadataLibrary.dylib
//
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -I %t -L %t -ltestMetadataLibrary -enable-experimental-feature Extern %s -o %t/VerifyExternalMetadata
// RUN: %target-codesign %t/VerifyExternalMetadata
//
// RUN: %host-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -I %swift_src_root/lib/ExternalGenericMetadataBuilder -L%swift-lib-dir -lswiftGenericMetadataBuilder -Xlinker -rpath -Xlinker %swift-lib-dir -enable-experimental-feature Extern %S/Inputs/buildMetadataJSON.swift -o %t/buildMetadataJSON
// RUN: %target-codesign %t/buildMetadataJSON
//
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %S/Inputs/json2c.swift -o %t/json2c
// RUN: %target-codesign %t/json2c
//
// RUN: %target-run %t/VerifyExternalMetadata getJSON > %t/names.json
// RUN: %target-run %t/buildMetadataJSON %target-arch %t/VerifyExternalMetadata %stdlib_dir/libswiftCore.dylib < %t/names.json > %t/libswiftPrespecialized.json
// RUN: %target-run %t/json2c %t/libswiftPrespecialized.json > %t/libswiftPrespecialized.c
// RUN: %clang -isysroot %sdk -target %target-triple -dynamiclib %t/libswiftPrespecialized.c -L%stdlib_dir -lswiftCore -o %t/libswiftPrespecialized.dylib
//
// Set a custom library path because we need to ensure we don't load an arm64e
// dylib into an arm64 test, since the prespecialized metadata depends on the
// exact contents of the library.
// RUN: env SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_LOGGING=y DYLD_INSERT_LIBRARIES=%t/libswiftPrespecialized.dylib SWIFT_DEBUG_LIB_PRESPECIALIZED_PATH=%t/libswiftPrespecialized.dylib %target-run env DYLD_LIBRARY_PATH=%stdlib_dir/%target-arch %t/VerifyExternalMetadata

// REQUIRES: executable_test
// REQUIRES: OS=macosx && CPU=arm64
// REQUIRES: rdar123810110

import ExternalGenericMetadataBuilder
import Foundation
import StdlibUnittest

import testMetadataLibrary

let args = CommandLine.arguments

if args.count > 1 && args[1] == "getJSON" {
  let types: [Any.Type] = [
    Array<Double>.self,
    GenericField<GenericField<Int8, Int16>,
                 Array<GenericStruct<Double, String, Float>>>.self,
    Array<Array<Array<Array<Array<Array<Array<Double>>>>>>>.self,
    testMetadataLibrary.Box<Int>.self,
    testMetadataLibrary.Box<String>.self,
    Box3<Int, Int, Int>.self,
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
