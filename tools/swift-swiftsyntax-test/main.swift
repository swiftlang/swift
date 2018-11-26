import SwiftSyntax
import Foundation
import TestUtils

struct InvalidArgumentValueError: Error, CustomStringConvertible {
  let argName: String
  let value: String

  var description: String {
    return "\(value) is not a valid value for \(argName)"
  }
}

/// Print the given message to stderr
func printerr(_ message: String, terminator: String = "\n") {
  FileHandle.standardError.write((message + terminator).data(using: .utf8)!)
}

/// Print the help message
func printHelp() {
  print("""
    Utility to test SwiftSyntax syntax tree deserialization.

    Actions (must specify one):
      -deserialize
            Deserialize a full pre-edit syntax tree (-pre-edit-tree) and write
            the source representation of the syntax tree to an out file (-out).
      -deserialize-incremental
            Deserialize a full pre-edit syntax tree (-pre-edit-tree), parse an
            incrementally transferred post-edit syntax tree (-incr-tree) and
            write the source representation of the post-edit syntax tree to an
            out file (-out).
      -classify-syntax
            Parse the given source file (-source-file) and output it with
            tokens classified for syntax colouring.
      -help
            Print this help message

    Arguments:
      -source-file FILENAME
            The path to a Swift source file to parse
      -pre-edit-tree FILENAME
            The path to a JSON serialized pre-edit syntax tree
      -incr-tree FILENAME
            The path to a JSON serialized incrementally transferred post-edit
            syntax tree
      -serialization-format {json,byteTree} [default: json]
            The format that shall be used to serialize/deserialize the syntax
            tree. Defaults to json.
      -out FILENAME
            The file to which the source representation of the post-edit syntax
            tree shall be written.
    """)
}

extension CommandLineArguments {
  func getSerializationFormat() throws -> SerializationFormat {
    switch self["-serialization-format"] {
    case nil:
      return .json
    case "json":
      return .json
    case "byteTree":
      return .byteTree
    default:
      throw InvalidArgumentValueError(
        argName: "-serialization-format",
        value: self["-serialization-format"]!
      )
    }
  }
}

func performDeserialize(args: CommandLineArguments) throws {
  let fileURL = URL(fileURLWithPath: try args.getRequired("-pre-edit-tree"))
  let outURL = URL(fileURLWithPath: try args.getRequired("-out"))
  let format = try args.getSerializationFormat()

  let fileData = try Data(contentsOf: fileURL)

  let deserializer = SyntaxTreeDeserializer()
  let tree = try deserializer.deserialize(fileData, serializationFormat: format)

  let sourceRepresenation = tree.description
  try sourceRepresenation.write(to: outURL, atomically: false, encoding: .utf8)
}

func performRoundTrip(args: CommandLineArguments) throws {
  let preEditTreeURL = URL(fileURLWithPath: try args.getRequired("-pre-edit-tree"))
  let incrTreeURL = URL(fileURLWithPath: try args.getRequired("-incr-tree"))
  let outURL = URL(fileURLWithPath: try args.getRequired("-out"))
  let format = try args.getSerializationFormat()

  let preEditTreeData = try Data(contentsOf: preEditTreeURL)
  let incrTreeData = try Data(contentsOf: incrTreeURL)

  let deserializer = SyntaxTreeDeserializer()
  _ = try deserializer.deserialize(preEditTreeData, serializationFormat: format)
  let tree = try deserializer.deserialize(incrTreeData,
                                          serializationFormat: format)
  let sourceRepresenation = tree.description
  try sourceRepresenation.write(to: outURL, atomically: false, encoding: .utf8)
}

func performClassifySyntax(args: CommandLineArguments) throws {
  let treeURL = URL(fileURLWithPath: try args.getRequired("-source-file"))

  let tree = try SyntaxTreeParser.parse(treeURL)
  let classifications = SyntaxClassifier.classifyTokensInTree(tree)
  let printer = ClassifiedSyntaxTreePrinter(classifications: classifications)
  let result = printer.print(tree: tree)

  if let outURL = args["-out"].map(URL.init(fileURLWithPath:)) {
    try result.write(to: outURL, atomically: false, encoding: .utf8)
  } else {
    print(result)
  }
}

do {
  let args = try CommandLineArguments.parse(CommandLine.arguments.dropFirst())

  if args.has("-deserialize-incremental") {
    try performRoundTrip(args: args)
  } else if args.has("-classify-syntax") {
    try performClassifySyntax(args: args)
  } else if args.has("-deserialize") {
    try performDeserialize(args: args)
  } else if args.has("-help") {
    printHelp()
  } else {
    printerr("""
      No action specified.
      See -help for information about available actions
      """)
    exit(1)
  }
  exit(0)
} catch {
  printerr("\(error)")
  printerr("Run swift-swiftsyntax-test -help for more help.")
  exit(1)
}
