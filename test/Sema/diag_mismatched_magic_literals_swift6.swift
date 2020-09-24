// The future "Swift 6 mode" behavior is staged in behind  `-enable-experimental-concise-pound-file`.
// RUN: %target-typecheck-verify-swift -enable-experimental-concise-pound-file

func callee(file: String = #file) {} // expected-note {{'file' declared here}}
func callee(fileID: String = #fileID) {} // expected-note {{'fileID' declared here}}
func callee(filePath: String = #filePath) {} // expected-note 2 {{'filePath' declared here}}

//
// #file equivalence
//
// These cases vary depending on -enable-experimental-concise-pound-file.
//

func passingToFile(fileID: String = #fileID, filePath: String = #filePath) {
  // expected-note@-1 {{did you mean for parameter 'filePath' to default to '#file'?}} {{65-74=#file}}

  callee(file: fileID)

  callee(file: filePath)
  // expected-warning@-1 {{parameter 'filePath' with default argument '#filePath' passed to parameter 'file', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{16-16=(}} {{24-24=)}}
}

func passingToFileID(file: String = #file, filePath: String = #filePath) {
  // expected-note@-1 {{did you mean for parameter 'filePath' to default to '#fileID'?}} {{63-72=#fileID}}

  callee(fileID: file)

  callee(fileID: filePath)
  // expected-warning@-1 {{parameter 'filePath' with default argument '#filePath' passed to parameter 'fileID', whose default argument is '#fileID'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{18-18=(}} {{26-26=)}}
}

func passingToFilePath(file: String = #file, fileID: String = #fileID) {
  // expected-note@-1 {{did you mean for parameter 'fileID' to default to '#filePath'?}} {{63-70=#filePath}}
  // expected-note@-2 {{did you mean for parameter 'file' to default to '#filePath'?}} {{39-44=#filePath}}

  callee(filePath: file)
  // expected-warning@-1 {{parameter 'file' with default argument '#file' passed to parameter 'filePath', whose default argument is '#filePath'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{20-20=(}} {{24-24=)}}

  callee(filePath: fileID)
  // expected-warning@-1 {{parameter 'fileID' with default argument '#fileID' passed to parameter 'filePath', whose default argument is '#filePath'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{20-20=(}} {{26-26=)}}
}
