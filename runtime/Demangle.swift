//===----------------------------------------------------------------------===//
// func demangle(mangled : String) -> String
//===----------------------------------------------------------------------===//

var substitutions : String[]
var genericParameters : String[]
var buildingArgumentList : Bool = false

// non-negative number:= <non-negative digit>+
func demangleNonNegativeNumber(i : Int, mangled : String)  -> (Int, Int) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, 0)
  }
  var c : Char = mangled[i]
  if !c.isDigit() {
    return (-1, 0)
  }
  var n = 0
  do {
    n = n * 10 + (c - '0')
    i = i + 1
    if i == sz {
      break;
    }
    c = mangled[i]
  } while c.isDigit()
  return (i, n)
}

// <positive number>:= <positive decimal digit><non-negative digit>*
func demanglePositiveNumber(i : Int, mangled : String)  -> (Int, Int) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, 0)
  }
  var c : Char = mangled[i]
  if !c.isDigit() || c == '0' {
    return (-1, 0)
  }
  var n = 0
  do {
    n = n * 10 + (c - '0')
    i = i + 1
    if i == sz {
      break;
    }
    c = mangled[i]
  } while c.isDigit()
  return (i, n)
}

// <power-of-2>:= One of 8 16 32 64 128
func demanglePowerOf2(i : Int, mangled : String)  -> (Int, Int) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, 0)
  }
  var c : Char = mangled[i]
  if c == '8' {
    return (i+1, 8)
  }
  if c == '1' {
    if i + 1 < sz {
      if mangled[i+1] == '6' {
        return (i+2, 16)
      }
      if mangled[i+1] == '2' {
        if i+2 < sz && mangled[i+2] == '8' {
          return (i+3, 128)
        }
      }
    }
    return (-1, 0)
  }
  if c == '3' {
    if i + 1 < sz && mangled[i+1] == '2' {
      return (i+2, 32)
    }
    return (-1, 0)
  }
  if c == '6' {
    if i + 1 < sz && mangled[i+1] == '4' {
      return (i+2, 64)
    }
    return (-1, 0)
  }
  return (-1, 0)
}

// S_
// S<non-negative number>_
func demangleSubstitution(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if sz - i < 2 {
    return (-1, mangled)
  }
  if mangled[i] == 'S' {
    if mangled[i+1] == '_' {
      return (i+2, substitutions[0])
    }
    var (j, n) = demangleNonNegativeNumber(i+1, mangled)
    if j > 0 && sz - j >= 1 && mangled[j] == '_' {
      return (j+1, substitutions[n+1])
    }
  }
  return (-1, mangled)
}

func addSubstitution(demangled : String)
{
  var newsubs = new String[substitutions.length+1]
  var i : Int
  for i = 0; i < substitutions.length; ++i {
    newsubs[i] = substitutions[i]
  }
  newsubs[i] = demangled
  substitutions = newsubs
}

func addGenericParameter() -> String
{
  if buildingArgumentList {
    var newParameters = new String[genericParameters.length+1]
    var i : Int
    for i = 0; i < genericParameters.length; ++i {
      newParameters[i] = genericParameters[i]
    }
    if i > 0 {
      newParameters[i] = String(genericParameters[genericParameters.length-1][0]+1)
    } else {
      newParameters[i] = "T"
    }
    genericParameters = newParameters
  }
  return genericParameters[genericParameters.length-1]
}

// <mangled operator char>:= 'a'  # &
//                           'd'  # /
//                           'e'  # =
//                           'g'  # >
//                           'l'  # <
//                           'm'  # *
//                           'n'  # !
//                           'o'  # |
//                           'p'  # +
//                           'r'  # %
//                           's'  # -
//                           't'  # ~
//                           'x'  # ^
//                           'z'  # .
func demangleOperator(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, mangled)
  }
  var c : Char = mangled[i]
  if c == 'a' {
    return (i+1, "&")
  } else if c == 'd' {
    return (i+1, "/")
  } else if c == 'e' {
    return (i+1, "=")
  } else if c == 'g' {
    return (i+1, ">")
  } else if c == 'l' {
    return (i+1, "<")
  } else if c == 'm' {
    return (i+1, "*")
  } else if c == 'n' {
    return (i+1, "!")
  } else if c == 'o' {
    return (i+1, "|")
  } else if c == 'p' {
    return (i+1, "+")
  } else if c == 'r' {
    return (i+1, "%")
  } else if c == 's' {
    return (i+1, "-")
  } else if c == 't' {
    return (i+1, "~")
  } else if c == 'x' {
    return (i+1, "^")
  } else if c == 'z' {
    return (i+1, ".")
  }
  return (-1, mangled)
}

// identifier:= <positive number><characters>+
//              'op' <positive number> <mangled operator char>
func demangleIdentifier(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, mangled)
  }
  var (j, n) = demanglePositiveNumber(i, mangled)
  if j > 0 {
    // <positive number><characters>+
    return (j+n, mangled[j .. j+n])
  }
  if sz - i >= 4 && mangled[i] == 'o' && mangled[i+1] == 'p' {
     (j, n) = demanglePositiveNumber(i+2, mangled)
     if (j > 0) {
        var s : String
        while n > 0 {
           var o : String
           (j, o) = demangleOperator(j, mangled)
           if j < 0 {
             return (-1, mangled)
           }
           s = s + o
           --n
        }
        return (j, s)
     }
  }
  return (-1, mangled)
}

// <decl name> := <context> <identifier>
func demangleDeclName(i : Int, mangled : String) -> (Int, String) {
  var (j, context) = demangleContext(i, mangled)
  if j > 0 {
    var (k, id) = demangleIdentifier(j, mangled)
    if k > 0 {
      if (context.isEmpty()) {
        return (k, id)
      }
      return (k, context + '.' + id)
    }
  }
  return (-1, mangled)
}

// TranslationUnit:= Ss
//                   context? identifier (substituable)

// NominalTypeDecl:= <decl name> (substituable)
//                   type

// ExtensionDecl:= type

// CapturingExpr:= <decl name>

// ConstructorDecl:= <decl name>

// TopLevelCodeDecl:= <decl name>

// context:= TranslationUnit
//           NominalTypeDecl
//           ExtensionDecl
//           CapturingExpr
//           ConstructorDecl
//           TopLevelCodeDecl
func demangleContext(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i+2 >= sz {
    return (-1, mangled)
  }
  if mangled[i] == 'S' {
    if mangled[i+1] == 's' {
        return (i+2, "")
    }
    return demangleSubstitution(i, mangled)
  }
  var (j, demangled) = demangleType(i, mangled)
  if j > 0 {
    return (j, demangled)
  }
  (j, demangled) = demangleIdentifier(i, mangled)
  if j > 0 {
    addSubstitution(demangled)
    return (j, demangled)
  }
  (j, demangled) = demangleDeclName(i, mangled)
  if j > 0 {
    addSubstitution(demangled)
    return (j, demangled)
  }
  return (-1, mangled)
}

// <type> ::= A <positive number><type> # fixed-sized arrays
// <type> ::= C <type>* _               # protocol composition (substitutable)
// <type> ::= F <type> <type>           # function type
// <type> ::= f <type> <type>           # uncurried function type
// <type> ::= f <power of 2>            # Builtin.Float
// <type> ::= i <power of 2>            # Builtin.Integer
// <type> ::= N <decl name>             # oneof, struct, or class (substitutable)
// <type> ::= O                         # Builtin.ObjCPointer
// <type> ::= o                         # Builtin.ObjectPointer
// <type> ::= P <decl name>             # protocol (substitutable)
// <type> ::= p                         # Builtin.RawPointer
// <type> ::= R <type>                  # lvalue
// <type> ::= T <tuple-element>* _      # tuple
// <type> ::= U <under construction> <function type> # generic-parameter-list type
// <type> ::= V                         # an archetype (provisional, obviously)
//
// <tuple-element> ::= <identifier>? <type>
func demangleType(i : Int, mangled : String) -> (Int, String) {
  var sz = mangled.size()
  if i >= sz {
    return (-1, mangled)
  }
  var c : Char = mangled[i]
  if c == 'F' {
    // function type:= F <type> <type>
    buildingArgumentList = true
    var (j, arguments) = demangleType(i+1, mangled)
    buildingArgumentList = false
    if j < 0 {
      return (-1, mangled)
    }
    var (k, return_t) = demangleType(j, mangled)
    if k < 0 {
      return (-1, mangled)
    }
    if return_t.size() < 0 || return_t == "()" {
      return (k, arguments)
    }
    return (k, arguments + " -> " + return_t)
  } else if c == 'f' {
    var (j1, n) = demanglePowerOf2(i+1, mangled)
    if j1 > 0 {
      // f <power of 2>       # Builtin.Float
      return (j1, "Builtin.Float" + mangled[i+1 .. j1])
    }
    // f <type> <type>           # uncurried function type
    buildingArgumentList = true
    var (j, arguments) = demangleType(i+1, mangled)
    buildingArgumentList = false
    if j < 0 {
      return (-1, mangled)
    }
    var (k, return_t) = demangleType(j, mangled)
    if k < 0 {
      return (-1, mangled)
    }
    return (k, "(" + arguments + ") -> " + return_t)
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
//     if mangled[i+1] == 'S' && (mangled[i+2] == '_' || mangled[i+2].isDigit()) {
//         return demangleSubstitution(i+1, mangled)
//     }
    var (j, decl) = demangleDeclName(i+1, mangled)
    if j < 0 {
      return (-1, mangled)
    }
    addSubstitution(decl)
    return (j, decl)
  } else if c == 'i' {
    // i <power of 2>           # Builtin.Integer
    var (j, n) = demanglePowerOf2(i+1, mangled)
    if (j > 0) {
      return (j, "Builtin.Integer" + mangled[i+1 .. j])
    }
    return (-1, mangled)
  } else if c == 'p' {
    // p                         # Builtin.RawPointer
    return (i+1, "Builtin.RawPointer")
  } else if c == 'o' {
    // o                         # Builtin.ObjectPointer
    return (i+1, "Builtin.ObjectPointer")
  } else if c == 'R' {
    // R <type>                  # lvalue
    var (j, type) = demangleType(i+1, mangled)
    if j > 0 {
      return (j, "[byref(heap)] " + type)
    }
  } else if c == 'U' {
    // U <under construction> <function type> # generic-parameter-list type
    var (j, f) = demangleType(i+1, mangled)
    if j > 0 {
      if genericParameters.isEmpty() {
        return (j, "<T>" + f)
      }
      var p = "<"
      p = p + genericParameters[0]
      for var h = 1; h < genericParameters.length; ++h {
        p = p + ", " + genericParameters[h]
      }
      p = p + ">"
      return (j, p + f)
    }
  } else if c == 'V' {
    // V                         # an archetype (provisional, obviously)
    return (i+1, addGenericParameter())
  } else if c == 'S' {
    return demangleSubstitution(i, mangled)
  }
  return (-1, mangled)
}

// witness-kind:= al  # AllocateBuffer
//                ac  # AssignWithCopy
//                at  # AssignWithTake
//                de  # DeallocateBuffer
//                xx  # Destroy
//                XX  # DestroyBuffer
//                CP  # InitializeBufferWithCopyOfBuffer
//                Cp  # InitializeBufferWithCopy
//                cp  # InitializeWithCopy
//                Tk  # InitializeBufferWithTake
//                tk  # InitializeWithTake
//                pr  # ProjectBuffer
//                sa  # SizeAndAlignment

// mangled-nmae:= '_Tw' <witness-kind> <type>
//                '_T' 'L'? <identifier>+ type? ['g' | 's']?
func demangle(mangled : String) -> String {
  substitutions = new String[0]
  genericParameters = new String[0]
  buildingArgumentList = false
  var sz = mangled.size()
  if sz < 3 {
    return mangled
  }
  if mangled[0] != '_' && mangled[1] != 'T' {
    return mangled
  }
  var start = 2
  var linkage : String
  if mangled[2] == 'L' {
    ++start
    linkage = " contains "
  }
  var (i , context) = demangleContext(start, mangled)
  if i < 0 {
    return mangled
  }
  if context == "" {
    context = "swift"
  }
  context = context + " : "
  var (j, identifier) = demangleIdentifier(i, mangled)
  if j < 0 {
    return mangled
  }
  if j == sz {
    return context + identifier
  }
  var type_kind : String
  var separator : String = ""
  if mangled[j] == 'F' || mangled[j] == 'f' || mangled[j] == 'U' {
    type_kind = "func"
  } else {
    type_kind = "var"
    separator = " : "
  }
  var (k, type) = demangleType(j, mangled)
  if k < 0 {
    return mangled
  }
  if k < sz {
    if mangled[k] == 'g' {
      if identifier == "__subscript" {
        identifier = "subscript"
      } 
      return context + identifier + ".get() -> " + type
    }
    if mangled[k] == 's' {
      if identifier == "__subscript" {
        identifier = "subscript"
      } 
      return context + identifier + ".set(" + type + ")"
    }
  }
  if identifier == "constructor" {
    return context + identifier + type
  }
  if linkage.isEmpty() {
    return context + type_kind + " " + identifier + separator + type
  }
  var (l, inner_id) = demangleIdentifier(k, mangled)
  if l < 0 {
    return mangled
  }
  var (m, inner_type) = demangleType(l, mangled)
  if m < 0 {
    return mangled
  }
  return context + type_kind + " " + identifier + separator + type + linkage +
         inner_id + inner_type
}
