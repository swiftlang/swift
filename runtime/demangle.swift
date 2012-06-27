//===----------------------------------------------------------------------===//
// func demangle(mangled : String) -> String
//===----------------------------------------------------------------------===//

var substitutions : String[]

func demangleSubstitution(i : Int, mangled : String) -> (Int, String) {
  if mangled[i] == 'S' {
    if mangled[i+1] == '_' {
      return (i+2, substitutions[0])
    }
    var j = i+1
    var c = mangled[j]
    if (!c.isDigit()) {
      return (-1, mangled)
    }
    var si = c - '0'
    ++j
    for c = mangled[j]; c != '_'; c = mangled[j] {
      si = si * 10 + (c - '0')
      ++j;
    }
    ++si
    ++j
    return (j, substitutions[si])
  }
  return (-1, mangled)
}

func addSubstitution(demangled : String)
{
  var newsubs = new String[substitutions.length+1]
  var i : Int
  for i = 0; i < substitutions.length; ++i {
    newsubs[i] = substitutions[i];
  }
  newsubs[i] = demangled;
  substitutions = newsubs;
}

func demangleIdentifier(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, mangled)
  }
  var c : Char = mangled[i]
  if !c.isDigit() {
    return (-1, mangled)
  }
  var len = 0
  do {
    len = len * 10 + (c - '0')
    i = i + 1
    if i == sz {
      return (-1, mangled)
    }
    c = mangled[i]
  } while c.isDigit()
  if len + i > sz {
    return (-1, mangled)
  }
  return (i+len, mangled[i .. i+len])
}

func demangleContext(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i+2 >= sz {
    return (-1, mangled)
  }
  if mangled[i] == 'S' {
    if mangled[i+1] == 's' {
        return (i+2, "swift")
    }
    return demangleSubstitution(i, mangled)
  }
  var (j, demangled) = demangleIdentifier(i, mangled)
  if j > 0 {
    addSubstitution(demangled)
  }
  return (j, demangled)
}

func demangleType(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, mangled)
  }
  var c : Char = mangled[i]
  if c == 'F' {
    // function type:= F <type> <type>
    var (j, arguments) = demangleType(i+1, mangled)
    if j < 0 {
      return (-1, mangled)
    }
    var (k, return_t) = demangleType(j, mangled)
    if k < 0 {
      return (-1, mangled)
    }
    if return_t.size() <= 2 {
      return (k, arguments)
    }
    return (k, arguments + " -> " + return_t)
  } else if c == 'T' {
    // tuple type:= <tuple-element>* _
    // <tuple-element>:= <identifier>? <type>
    var j = i + 1
    if j == sz {
      return (-1, mangled)
    }
    c = mangled[j]
    var numElements = 0
    var demangled = "("
    while c != '_' {
      var (k, id) = demangleIdentifier(j, mangled)
      var (l, type) = demangleType(max(k, j), mangled)
      if l < 0 {
        return (-1, mangled)
      }
      numElements = numElements + 1
      if numElements > 1 {
        demangled = demangled + ", "
      }
      if k > 0 {
        demangled = demangled + id + " : "
      }
      demangled = demangled + type
      j = l
      c = mangled[j]
    }
    return (j+1, demangled + ")")
  } else if c == 'N' {
    // N <decl> # oneof, struct, or class (substitutable)
    if mangled[i+1] == 'S' && (mangled[i+2] == '_' || mangled[i+2].isDigit()) {
        return demangleSubstitution(i+1, mangled)
    }
    var (j, context) = demangleContext(i+1, mangled)
    if j < 0 {
      return (-1, mangled)
    }
    var (k, id) = demangleIdentifier(j, mangled)
    if k < 0 {
      return (-1, mangled)
    }
    if context != "swift" {
      id = context + "::" + id
    }
    addSubstitution(id)
    return (k, id)
  } else if c == 'S' {
    return demangleSubstitution(i, mangled)
  }
  return (-1, mangled)
}

func demangle(mangled : String) -> String {
  var sz = mangled.size()
  if sz < 3 {
    return mangled
  }
  if mangled[0] != '_' && mangled[1] != 'T' {
    return mangled
  }
  var (i , context) = demangleContext(2, mangled)
  if i < 0 {
    return mangled
  }
  context = context + ".swift : "
  var (j, identifier) = demangleIdentifier(i, mangled)
  if j < 0 {
    return mangled
  }
  var (k, type) = demangleType(j, mangled)
  if k < 0 {
    return mangled
  }
  return context + "func " + identifier + type
}
