var _argv : Vector<String> = new Vector<String>

var argv : Vector<String> {
     if _argv.length == 0 {
         for i in 0..Int(C_ARGC) {
             _argv.append(String.fromCString(C_ARGV[i]))
         }
     }
     return _argv
}
