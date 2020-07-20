// RUN: %target-typecheck-verify-swift

func callee(file: String = #file) {}
func callee(fileID: String = #fileID) {} // expected-note {{'fileID' declared here}}
func callee(filePath: String = #filePath) {} // expected-note {{'filePath' declared here}}

//
// #file equivalence
//
// These cases vary depending on -enable-experimental-concise-pound-file.
//

func passingToFile(fileID: String = #fileID, filePath: String = #filePath) {
  callee(file: fileID)

  callee(file: filePath)
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

  callee(filePath: file)

  callee(filePath: fileID)
  // expected-warning@-1 {{parameter 'fileID' with default argument '#fileID' passed to parameter 'filePath', whose default argument is '#filePath'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{20-20=(}} {{26-26=)}}
}
