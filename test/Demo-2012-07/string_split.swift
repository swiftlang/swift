// RUN: false
// REQUIRES: disabled

// FIXME: Remove this once we have vector.
import vectorstring

extension String {
  // FIXME: Move this into the standard library.
  func split(arg : Char) -> String[] {
    // Create a vector to hold the result.
    var lines = VectorString()
    var contents = this
    while true {
      var split = contents.splitFirst(arg)
      lines.append(split.before)
      if (!split.wasFound) { break }
      contents = split.after
    }

    // Convert the vector to an array.
    //
    // FIXME: Remove this.
    var linesArray = new String[lines.length]
    for i in 0..lines.length {
      linesArray[i] = lines[i]
    }

    return linesArray
  }
}
