import SwiftSyntax
import Foundation

/// Print the given message to stderr
func printerr(_ message: String, terminator: String = "\n") {
  FileHandle.standardError.write((message + terminator).data(using: .utf8)!)
}

/// Print the help message
func printHelp() {
  print("""
    Utility to test SwiftSyntax syntax tree deserialization.

    Actions (must specify one):
      --deserialize-incremental
            Deserialize a full pre-edit syntax tree (--pre-edit-tree), parse an
            incrementally transferred post-edit syntax tree (--incr-tree) and
            write the source representation of the post-edit syntax tree to an
            out file (--out).
      --help
            Print this help message

    Arguments:
      --pre-edit-tree FILENAME
            The path to a JSON serialized pre-edit syntax tree
      --incr-tree FILENAME
            The path to a JSON serialized incrementally transferred post-edit
            syntax tree
      --out FILENAME
            The file to which the source representation of the post-edit syntax
            tree shall be written.
    """)
}

func performRoundTrip(args: CommandLineArguments) throws {
  let preEditTreeURL = URL(fileURLWithPath: try args.getRequired("--pre-edit-tree"))
  let incrTreeURL = URL(fileURLWithPath: try args.getRequired("--incr-tree"))
  let outURL = URL(fileURLWithPath: try args.getRequired("--out"))

  let preEditTreeData = try Data(contentsOf: preEditTreeURL)
  let incrTreeData = try Data(contentsOf: incrTreeURL)

  let deserializer = SyntaxTreeDeserializer()
  _ = try deserializer.deserialize(preEditTreeData)
  let tree = try deserializer.deserialize(incrTreeData)
  let sourceRepresenation = tree.description
  try sourceRepresenation.write(to: outURL, atomically: false, encoding: .utf8)
}

do {
  let args = try CommandLineArguments.parse(CommandLine.arguments.dropFirst())

  if args.has("--deserialize-incremental") {
    try performRoundTrip(args: args)
    exit(0)
  } else if args.has("--help") {
    printHelp()
    exit(0)
  } else {
    printerr("""
      No action specified.
      See --help for information about available actions
      """)
    exit(1)
  }
} catch {
  printerr(error.localizedDescription)
  printerr("Run swift-swiftsyntax-test --help for more help.")
  exit(1)
}
