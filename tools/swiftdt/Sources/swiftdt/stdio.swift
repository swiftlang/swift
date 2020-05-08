import Darwin

enum Std {
  struct File: TextOutputStream {
    var underlying: UnsafeMutablePointer<FILE>

    mutating func write(_ string: String) {
      fputs(string, underlying)
    }
  }
  
  static var err = File(underlying: stderr)
}
