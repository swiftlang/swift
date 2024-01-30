import ExternalGenericMetadataBuilder
import Foundation

var args = CommandLine.arguments
args.removeFirst() // Skip the program name.

let arch = args.removeFirst()
let dylibs = args

let builder = swift_externalMetadataBuilder_create(1, arch)

guard var jsonIn = try! FileHandle.standardInput.readToEnd() else {
  fatalError("No data read from stdin, somehow")
}

// NULL terminate.
jsonIn.append(contentsOf: [0])

let readJSONErrorCStr = jsonIn.withUnsafeBytes {
  swift_externalMetadataBuilder_readNamesJSON(builder, $0)
}

if let readJSONErrorCStr {
  fatalError("Could not read JSON input: \(String(cString: readJSONErrorCStr))")
}

for dylib in dylibs {
  let url = URL(fileURLWithPath: dylib)
  let data = NSData(contentsOf: url)!

  let machHeader = data.bytes.assumingMemoryBound(to: mach_header.self)
  let addDylibErrorCStr =
    swift_externalMetadataBuilder_addDylib(builder,
                                           url.lastPathComponent,
                                           machHeader,
                                           UInt64(data.length));
  if let addDylibErrorCStr {
    fatalError("Could not add dylib at \(dylib): \(String(cString: addDylibErrorCStr))")
  }
}

let buildErrorCStr = swift_externalMetadataBuilder_buildMetadata(builder)
if let buildErrorCStr {
  fatalError("Failed to build metadata: \(String(cString: buildErrorCStr))")
}

let outputJSONCStr = swift_externalMetadataBuilder_getMetadataJSON(builder)
if outputJSONCStr == nil {
  fatalError("JSON creation failed")
}

fputs(outputJSONCStr, stdout)
