//===----------------------------------------------------------------------===//
// func demangle(mangled : String) -> String
//===----------------------------------------------------------------------===//

// Fake up a monad 'bind' operator to help thread mangling failures.
operator infix >>- {
  associativity right
}

func >>- (s:(String,Bool), f:(String) -> (String, Bool))
-> (String,Bool) {
  if s.1 {
    return f(s.0)
  }
  return s
}

struct Demangler {
  struct Substitution {
    var name : String
    var isProtocol : Bool
  }

  var substitutions : Vector<Substitution>
  var archetypeCounts : Vector<Int>
  var archetypeCount : Int
  var mangled : String
  var offset : Int

  constructor(mangled:String) {
    this.substitutions = Vector<Substitution>()
    this.archetypeCounts = Vector<Int>()
    this.archetypeCount = 0
    this.mangled = mangled
    this.offset = 0
  }

  func parseInt(s:String) -> Int {
    // NB: Assumes s is a valid decimal integer
    var x = 0
    for c in s.chars {
      x = 10*x + (c - '0')
    }
    return x
  }

  func empty() -> Bool {
    return offset >= mangled.length
  }

  func peekNext() -> Char {
    assert(offset < mangled.length)
    return mangled[offset]
  }

  func next() -> Char {
    assert(offset < mangled.length)
    return mangled[offset++]
  }

  func nextIf(c:Char) -> Bool {
    if empty() {
      return false
    }
    var next = mangled[offset]
    if c == next {
      ++offset
      return true
    }
    return false
  }

  func isStartOfIdentifier(c:Char) -> Bool {
    // TODO: 'X' indicates the start of a Punycode-encoded Unicode identifier
    return (c >= '0' && c <= '9') || c == 'o'
  }

  func isStartOfNominalType(c:Char) -> Bool {
    return c == 'C' || c == 'V' || c == 'O'
  }

  func failed() -> (String, Bool) {
    //println("failed") //DEBUG
    return (mangled, false)
  }
  func succeeded(s:String) -> (String, Bool) {
    //println("succeeded \(s)") //DEBUG
    return (s, true)
  }

  func withSubstitution(s:String, isProtocol:Bool) -> (String, Bool) {
    //println("subst \(substitutions.length) => \(s) \(isProtocol)") //DEBUG
    substitutions.append(Substitution(s, isProtocol))
    return succeeded(s)
  }

  // mangled-name ::= '_T' global
  func demangleSymbol() -> (String, Bool) {
    //println("symbol \(offset)") //DEBUG
    if offset + 2 > mangled.length {
      return failed()
    }
    if mangled[offset..offset+2] != "_T" {
      return failed()
    }

    // global ::= 'To' global                     // swift-as-ObjC thunk
    if offset + 4 <= mangled.length
      && mangled[offset..offset+4] == "_TTo" {
      offset += 4
      return demangleGlobal() >>- { succeeded("[objc] \($0)") }
    }

    offset += 2
    return demangleGlobal()
  }

  func demangleGlobal() -> (String, Bool) {
    //println("global \(offset)") //DEBUG
    if empty() {
      return failed()
    }

    if nextIf('M') {
      // global ::= 'MP' directness type            // type metadata pattern
      if nextIf('P') {
        return demangleDirectness()
          >>- {|directness| demangleType()
          >>- {|type| succeeded("\(directness) generic type metadata pattern for \(type)") }}
      }
      // global ::= 'Mm' type                       // class metaclass
      if nextIf('m') {
        return demangleType()
          >>- {|type| succeeded("metaclass for \(type)") }
      }
      // global ::= 'M' directness type             // type metadata
      return demangleDirectness()
        >>- {|directness| demangleType()
        >>- {|type| succeeded("\(directness) type metadata for \(type)") }}
    }

    // global ::= 'nk_' entity                    // protocol witness
    if nextIf('n') {
      if nextIf('k') && nextIf('_') {
        return demangleEntity()
          >>- {|entity| succeeded("protocol witness for \(entity)") }
      }
      return failed()
    }

    // global ::= 'w' value-witness-kind type     // value witness
    if nextIf('w') {
      return demangleValueWitnessKind()
        >>- {|kind| demangleType()
        >>- {|type| succeeded("\(kind) value witness for \(type)") }}
    }

    if nextIf('W') {
      // global ::= 'WV' type                       // value witness table
      if nextIf('V') {
        return demangleType()
          >>- {|type| succeeded("value witness table for \(type)") }
      }
      // global ::= 'Wo' entity                     // witness table offset
      if nextIf('o') {
        return demangleEntity()
          >>- {|entity| succeeded("witness table offset for \(entity)") }
      }
      // global ::= 'Wv' directness entity          // field offset
      if nextIf('v') {
        return demangleDirectness()
          >>- {|directness| demangleEntity()
          >>- {|entity| succeeded("\(directness) field offset for \(entity)") }}
      }
      return failed()
    }

    // global ::= 'Tb' type                       // swift-to-ObjC block converter
    if nextIf('T') {
      if nextIf('b') {
        return demangleType()
          >>- {|type| succeeded("bridge-to-block function for \(type)") }
      }
      return failed()
    }

    // global ::= local-marker? entity                  // some identifiable thing
    // local-marker ::= 'L'
    if nextIf('L') {
      return demangleEntity() >>- {succeeded("local \($0)")}
    }

    return demangleEntity()
  }

  // directness ::= 'd'                         // direct
  // directness ::= 'i'                         // indirect
  func demangleDirectness() -> (String, Bool) {
    if nextIf('d') {
      return succeeded("direct")
    }
    if nextIf('i') {
      return succeeded("indirect")
    }
    return failed()
  }

  // value-witness-kind ::= 'al'                // allocateBuffer
  // value-witness-kind ::= 'ca'                // assignWithCopy
  // value-witness-kind ::= 'ta'                // assignWithTake
  // value-witness-kind ::= 'de'                // deallocateBuffer
  // value-witness-kind ::= 'xx'                // destroy
  // value-witness-kind ::= 'XX'                // destroyBuffer
  // value-witness-kind ::= 'CP'                // initializeBufferWithCopyOfBuffer
  // value-witness-kind ::= 'Cp'                // initializeBufferWithCopy
  // value-witness-kind ::= 'cp'                // initializeWithCopy
  // value-witness-kind ::= 'Tk'                // initializeBufferWithTake
  // value-witness-kind ::= 'tk'                // initializeWithTake
  // value-witness-kind ::= 'pr'                // projectBuffer
  // value-witness-kind ::= 'ty'                // typeof
  func demangleValueWitnessKind() -> (String, Bool) {
    if empty() { return failed() }
    var c1 = next()
    if empty() { return failed() }
    var c2 = next()
    var s = "" + c1 + c2
    if s == "al" {
      return succeeded("allocateBuffer")
    }
    if s == "ca" {
      return succeeded("assignWithCopy")
    }
    if s == "ta" {
      return succeeded("assignWithTake")
    }
    if s == "de" {
      return succeeded("deallocateBuffer")
    }
    if s == "xx" {
      return succeeded("destroy")
    }
    if s == "XX" {
      return succeeded("destroyBuffer")
    }
    if s == "CP" {
      return succeeded("initializeBufferWithCopyOfBuffer")
    }
    if s == "Cp" {
      return succeeded("initializeBufferWithCopy")
    }
    if s == "cp" {
      return succeeded("initializeWithCopy")
    }
    if s == "Tk" {
      return succeeded("initializeBufferWithTake")
    }
    if s == "tk" {
      return succeeded("initializeWithTake")
    }
    if s == "pr" {
      return succeeded("projectBuffer")
    }
    if s == "ty" {
      return succeeded("typeof")
    }
    return failed()
  }

  // entity ::= context 'D'                     // deallocating destructor
  // entity ::= context 'd'                     // non-deallocating destructor
  // entity ::= context 'C' type                // allocating constructor
  // entity ::= context 'c' type                // non-allocating constructor
  // entity ::= declaration 'g'                 // getter
  // entity ::= declaration 's'                 // setter
  // entity ::= declaration 'a'                 // addressor
  // entity ::= declaration                     // other declaration
  func demangleEntity() -> (String, Bool) {
    //println("entity \(offset)") //DEBUG
    return demangleContext() >>- {|context|
      if nextIf('D') {
        return succeeded("\(context).destructor")
      }
      if nextIf('d') {
        return succeeded("\(context).destructor [destroying]")
      }
      if nextIf('C') {
        return demangleType() >>-
          {succeeded("\(context).constructor : \($0)")}
      }
      if nextIf('c') {
        return demangleType() >>-
          {succeeded("\(context).constructor [initializing] : \($0)")}
      }

      return demangleDeclWithContext(context) >>- {|decl|
        if nextIf('a') {
          return succeeded("\(decl) addressor")
        }
        if nextIf('g') {
          return succeeded("\(decl) getter")
        }
        if nextIf('s') {
          return succeeded("\(decl) setter")
        }
        return succeeded(decl)
      }
    }
  }

  // declaration ::= context identifier type
  func demangleDecl() -> (String, Bool) {
    //println("decl \(offset)") //DEBUG
    return demangleContext() >>- {demangleDeclWithContext($0)}
  }

  func demangleDeclWithContext(context:String) -> (String, Bool) {
    //println("declWithContext \(offset)") //DEBUG
    return demangleIdentifier()
      >>- {|name| demangleDeclTypeWithContextAndName(context, name) }
  }

  func demangleDeclTypeWithContextAndName(context:String, name:String)
    -> (String, Bool)
  {
    //println("declWithContextAndName \(offset)") //DEBUG
    return demangleType()
      >>- {|type| succeeded("\(context).\(name) : \(type)") }
  }

  // NB: Left-recursion entity->context->entity in 'function' case
  // context ::= module
  // TODO: context ::= function
  // context ::= nominal-type
  // context ::= protocol-context
  // TODO: function ::= entity
  func demangleContext() -> (String, Bool) {
    //println("context \(offset)") //DEBUG
    if empty() { return failed() }
    var c = peekNext()
    if isStartOfIdentifier(c) || c == 'S' {
      return demangleModule()
    }
    if isStartOfNominalType(c) {
      return demangleNominalType()
    }
    // protocol-context ::= 'P' protocol
    if c == 'P' {
      next()
      return demangleProtocolName()
    }
    return failed()
  }

  // module ::= substitution                    // other substitution
  // module ::= identifier                      // module name
  // module ::= known-module                    // abbreviation
  func demangleModule() -> (String, Bool) {
    //println("module \(offset)") //DEBUG
    var c = peekNext()
    if isStartOfIdentifier(c) {
      return demangleIdentifier() >>- {withSubstitution($0, isProtocol:false)}
    }
    if c == 'S' {
      return demangleSubstitution()
    }
    return failed()
  }

  func parenthesize(s:String) -> String {
    if s.startsWith("(") {
      return s
    }
    return "(\(s))"
  }

  func demangleBuiltinSize() -> (String, Bool) {
    return demangleNatural()
      >>- {|size| nextIf('_') ? succeeded(size) : failed() }
  }

  func demangleType() -> (String, Bool) {
    //println("type \(offset)") //DEBUG
    if empty() {
      return failed()
    }
    var c = next()
    // type ::= 'A' natural type                  // fixed-size array
    if c == 'A' {
      return demangleNatural()
        >>- {|size| demangleType()
        >>- {|base| succeeded("\(base)[\(size)]") }}
    }
    if c == 'B' {
      if empty() {
        return failed()
      }
      c = next()
      // type ::= 'Bf' natural '_'                  // Builtin.Float
      if c == 'f' {
        return demangleBuiltinSize()
          >>- {|size| succeeded("Builtin.Float\(size)") }
      }
      // type ::= 'Bi' natural '_'                  // Builtin.Integer
      if c == 'i' {
        return demangleBuiltinSize()
          >>- {|size| succeeded("Builtin.Int\(size)") }
      }
      // type ::= 'BO'                              // Builtin.ObjCPointer
      if c == 'O' {
        return succeeded("Builtin.ObjCPointer")
      }
      // type ::= 'Bo'                              // Builtin.ObjectPointer
      if c == 'o' {
        return succeeded("Builtin.ObjectPointer")
      }
      // type ::= 'Bp'                              // Builtin.RawPointer
      if c == 'p' {
        return succeeded("Builtin.RawPointer")
      }
      // type ::= 'Bu'                              // Builtin.OpaquePointer
      if c == 'u' {
        return succeeded("Builtin.OpaquePointer")
      }
      // type ::= 'Bv' natural type                 // Builtin.Vec<n>x<type>
      if c == 'v' {
        return demangleNatural()
          >>- {|elts|
            // Only Builtin.Int/Float/RawPointer types should be used as
            // vector elts.
            if !nextIf('B') {
              return failed()
            }
            if nextIf('i') {
              return demangleBuiltinSize()
                >>- {|size| succeeded("Builtin.Vec\(elts)xInt\(size)") }
            }
            if nextIf('f') {
              return demangleBuiltinSize()
                >>- {|size| succeeded("Builtin.Vec\(elts)xFloat\(size)") }
            }
            if nextIf('p') {
              return succeeded("Builtin.Vec\(elts)xRawPointer")
            }
          }
      }
      return failed()
    }
    // type ::= 'b' type type                     // objc block function type
    if c == 'b' {
      return demangleType()
        >>- {|in| demangleType()
        >>- {|out| succeeded("[objc_block] \(parenthesize(in)) -> \(out)") }}
    }
    // type ::= 'F' type type                     // function type
    if c == 'F' {
      return demangleType()
        >>- {|in| demangleType()
        >>- {|out| succeeded("\(parenthesize(in)) -> \(out)") }}
    }
    // type ::= 'f' type type                     // uncurried function type
    if c == 'f' {
      return demangleType()
        >>- {|in| demangleType()
        >>- {|out| succeeded("\(parenthesize(in))\(out)") }}
    }
    // type ::= 'G' type <type>+ '_'              // generic type application
    if c == 'G' {
      return demangleType()
        >>- {|base| demangleList({demangleType()})
        >>- {|params| succeeded("\(base)<\(params)>") }}
    }
    // type ::= 'M' type                          // metatype
    if c == 'M' {
      return demangleType() >>- {succeeded("\($0).metatype")}
    }
    // type ::= 'P' protocol-list '_'             // protocol
    if c == 'P' {
      return demangleProtocolList()
    }
    // type ::= 'Q' index                   // archetype with depth=0
    // type ::= 'Qd' index index            // archetype with depth=M+1
    if c == 'Q' {
      if nextIf('d') {
        return demangleIndex()
          >>- {|depthStr| demangleIndex()
          >>- {|indexStr| demangleArchetypeRef(parseInt(depthStr)+1,
                                               parseInt(indexStr)) }}
      }
      return demangleIndex()
        >>- {|indexStr| demangleArchetypeRef(0, parseInt(indexStr)) }
    }
    // type ::= 'R' type                          // byref
    if c == 'R' {
      return demangleType() >>- {succeeded("[byref] \($0)")}
    }
    // type ::= 'T' tuple-element* '_'            // tuple
    if c == 'T' {
      return demangleTuple(variadic:false)
    }
    // type ::= 't' tuple-element* '_'            // variadic tuple
    if c == 't' {
      return demangleTuple(variadic:true)
    }
    // type ::= nominal-type
    if isStartOfNominalType(c) {
      return demangleDeclarationName(isProtocol:false)
    }
    if c == 'S' {
      return demangleSubstitutionIndex()
    }
    // type ::= 'U' generics '_' type             // generic type
    if c == 'U' {
      archetypeCounts.append(archetypeCount)
      var result = demangleArchetypes()
        >>- {|archetypes| demangleType()
        >>- {|base| succeeded("<\(archetypes)> \(base)") }}
      archetypeCount = archetypeCounts[archetypeCounts.length - 1]
      archetypeCounts.popBack()
      return result
    }
    return failed()
  }

  func archetypeName(i:Int) -> String {
    var name = ""
    do {
      name += 'A' + i % 26
      i /= 26
    } while i != 0
    return name
  }

  func demangleArchetypeRef(depth:Int, i:Int) -> (String, Bool) {
    // Assume an implicit archetype set if no generic types have been explicitly
    // opened. This lets us demangle archetypes referenced in generic type
    // property accessors.
    if depth == 0 && archetypeCount == 0 {
      return succeeded(archetypeName(i))
    }

    if depth >= archetypeCounts.length {
      return failed()
    }
    var index = archetypeCounts[archetypeCounts.length - 1 - depth] + i
    var max = depth == 0
      ? archetypeCount
      : archetypeCounts[archetypeCounts.length - depth]
    if index >= max {
      return failed()
    }
    return succeeded(archetypeName(index))
  }

  // generics ::= generic-parameter+
  // generic-parameter ::= protocol-list '_'
  // protocol-list ::= protocol*
  func demangleArchetypes() -> (String, Bool) {
    var result = ""
    var first = true
    while true {
      if nextIf('_') {
        // This is the terminating '_' if it isn't followed by another
        // protocol list.
        if empty() { return failed() }
        var c = peekNext()
        if c != '_' && c != 'S' && !isStartOfIdentifier(c) {
          break
        }

        if !first {
          result += ", "
        }

        result += "\(archetypeName(archetypeCount))"
      } else {
        var (protocols, ok) = demangleProtocolList()
        if !ok { return failed() }

        if !first {
          result += ", "
        }

        result += "\(archetypeName(archetypeCount)) : \(protocols)"
      }

      ++archetypeCount
      first = false
    }

    return succeeded(result)
  }

  func demangleList(element : () -> (String, Bool)) -> (String, Bool) {
    var result = ""
    var first = true
    while !nextIf('_') {
      if !first {
        result += ", "
      }
      var (next, ok) = element()
      if !ok { return failed() }
      result += next
      first = false
    }
    return succeeded(result)
  }

  // type ::= 'T' tuple-element* '_'            // tuple
  // tuple-element ::= identifier? type
  func demangleTuple(variadic:Bool) -> (String, Bool) {
    return demangleList({
      if empty() { return failed() }
      return isStartOfIdentifier(peekNext())
        ? demangleIdentifier()
            >>- {|id| demangleType()
            >>- {|ty| succeeded("\(id) : \(ty)") }}
        : demangleType()
    }) >>- {|list|
      var ellipsis = variadic ? "..." : ""
      return succeeded("(\(list)\(ellipsis))")
    }
  }

  // nominal-type ::= known-nominal-type
  // nominal-type ::= substitution
  // nominal-type ::= nominal-type-kind declaration-name
  func demangleNominalType() -> (String, Bool) {
    //println("nominalType \(offset)") //DEBUG
    if empty() {
      return failed()
    }
    var c = next()
    if c == 'S' {
      return demangleSubstitutionIndex()
    }
    if isStartOfNominalType(c) {
      return demangleDeclarationName(isProtocol:false)
    }
    return failed()
  }

  // declaration-name ::= context identifier
  func demangleDeclarationName(isProtocol:Bool) -> (String, Bool) {
    //println("nominalTypeName \(offset)") //DEBUG
    return demangleContext()
      >>- {|context| demangleIdentifier()
      >>- {|identifier| withSubstitution("\(context).\(identifier)",
                                         isProtocol) }}
  }

  // protocol ::= substitution
  // protocol ::= declaration-name
  func demangleProtocolName() -> (String, Bool) {
    // A substitution can refer either to a protocol by itself or to a context
    // in which a protocol is named.
    if nextIf('S') {
      var ((result, ok), isProtocol) = demangleSubstitutionIndexWithProtocol()
      if !ok { return failed() }
      if isProtocol {
        return succeeded(result)
      }
      return demangleIdentifier()
        >>- {|identifier| withSubstitution("\(result).\(identifier)",
                                           isProtocol:true) }
    }
    return demangleDeclarationName(isProtocol:true)
  }

  func demangleProtocolList() -> (String, Bool) {
    if nextIf('_') {
      return succeeded("protocol<>")
    }
    var (first, ok) = demangleProtocolName()
    if !ok { return failed() }
    if nextIf('_') {
      return succeeded(first)
    }

    return demangleList({demangleProtocolName()})
      >>- { succeeded("protocol<\(first), \($0)>") }
  }

  // identifier ::= natural identifier-start-char identifier-char*
  // identifier ::= 'o' operator-fixity natural operator-char+
  // TODO: identifier ::= 'X' natural punycode-string
  // TODO: identifier ::= 'Xo' operator-fixity natural punycode-string
  func demangleIdentifier() -> (String, Bool) {
    //println("identifier \(offset)") //DEBUG
    if nextIf('o') {
      // operator-fixity ::= 'p'                    // prefix operator
      // operator-fixity ::= 'P'                    // postfix operator
      // operator-fixity ::= 'i'                    // infix operator
      // FIXME: Attribute should go before the complete identifier in context
      if nextIf('p') {
        return demangleOperator() >>- {succeeded("\($0) [prefix]")}
      }
      if nextIf('P') {
        return demangleOperator() >>- {succeeded("\($0) [postfix]")}
      }
      if nextIf('i') {
        return demangleOperator() >>- {succeeded("\($0) [infix]")}
      }
      return failed()
    }
    return demangleNatural() >>- {|lengthStr|
      var length = parseInt(lengthStr)
      if (offset + length > mangled.length) {
        return failed()
      }
      var s = mangled[offset..offset+length]
      offset += length
      return succeeded(s)
    }
  }

  // operator-char ::= 'a'                      // &
  // operator-char ::= 'c'                      // @
  // operator-char ::= 'd'                      // /
  // operator-char ::= 'e'                      // =
  // operator-char ::= 'g'                      // >
  // operator-char ::= 'l'                      // <
  // operator-char ::= 'm'                      // *
  // operator-char ::= 'n'                      // !
  // operator-char ::= 'o'                      // |
  // operator-char ::= 'p'                      // +
  // operator-char ::= 'r'                      // %
  // operator-char ::= 's'                      // -
  // operator-char ::= 'x'                      // ^
  // operator-char ::= 't'                      // ~
  // operator-char ::= 'z'                      // .
  func operatorCharTable() -> String {
    //      abcdefghijklmnopqrstuvwxyz
    return "& @/= >    <*!|+ %-~   ^ ."
  }

  func demangleOperator() -> (String, Bool) {
    //println("operator \(offset)") //DEBUG
    return demangleNatural() >>- {|lengthStr|
      var length = parseInt(lengthStr)
      if (offset + length > mangled.length) {
        return failed()
      }
      var substr = mangled[offset..offset+length]
      var op = ""
      offset += length

      for c in substr.chars {
        if c < 'a' || c > 'z' {
          return failed()
        }
        var o = operatorCharTable()[c - 'a']
        if o == ' ' {
          return failed()
        }
        op += o
      }
      return succeeded(op)
    }
  }

  // substitution ::= 'S' index
  // known-module ::= 'So'                      // Objective-C
  // known-module ::= 'Ss'                      // swift
  // known-nominal-type ::= 'Sa'                // swift.Slice
  // known-nominal-type ::= 'Sb'                // swift.Bool
  // known-nominal-type ::= 'Sc'                // swift.Char
  // known-nominal-type ::= 'Sd'                // swift.Float64
  // known-nominal-type ::= 'Sf'                // swift.Float32
  // known-nominal-type ::= 'Si'                // swift.Int64
  // known-nominal-type ::= 'SS'                // swift.String
  // known-nominal-type ::= 'Su'                // swift.UInt64
  func demangleSubstitution() -> (String, Bool) {
    //println("subst \(offset)") //DEBUG
    if nextIf('S') {
      return demangleSubstitutionIndex()
    }
    return failed()
  }

  func demangleSubstitutionIndexWithProtocol()
  -> (result:(String, Bool), isProtocol:Bool) {
    //println("substIndex \(offset)") //DEBUG
    if empty() {
      return (result:failed(), isProtocol:false)
    }
    if nextIf('o') {
      return (result:succeeded("ObjectiveC"), isProtocol:false)
    }
    if nextIf('s') {
      return (result:succeeded("swift"), isProtocol:false)
    }
    if nextIf('a') {
      return (result:succeeded("swift.Slice"), isProtocol:false)
    }
    if nextIf('b') {
      return (result:succeeded("swift.Bool"), isProtocol:false)
    }
    if nextIf('c') {
      return (result:succeeded("swift.Char"), isProtocol:false)
    }
    if nextIf('d') {
      return (result:succeeded("swift.Float64"), isProtocol:false)
    }
    if nextIf('f') {
      return (result:succeeded("swift.Float32"), isProtocol:false)
    }
    if nextIf('i') {
      return (result:succeeded("swift.Int64"), isProtocol:false)
    }
    if nextIf('S') {
      return (result:succeeded("swift.String"), isProtocol:false)
    }
    if nextIf('u') {
      return (result:succeeded("swift.UInt64"), isProtocol:false)
    }
    var (indexStr, ok) = demangleIndex()
    if !ok { return (result:failed(), isProtocol:false) }
    var index = parseInt(indexStr)
    if index < 0 || index >= substitutions.length {
      return (result:failed(), isProtocol:false)
    }
    return (result:succeeded(substitutions[index].name),
            isProtocol:substitutions[index].isProtocol)
  }

  func demangleSubstitutionIndex() -> (String, Bool) {
    var (result, isProtocol) = demangleSubstitutionIndexWithProtocol()
    return result
  }

  // index ::= '_'                              // 0
  // index ::= natural '_'                      // N+1
  func demangleIndex() -> (String, Bool) {
    //println("index \(offset)") //DEBUG
    if nextIf('_') {
      return succeeded("0")
    }
    return demangleNatural() >>- {|indexStr|
      if !nextIf('_') {
        return failed()
      }
      return succeeded("\(parseInt(indexStr)+1)")
    }
  }

  // natural ::= [0-9]+
  func demangleNatural() -> (String, Bool) {
    //println("natural \(offset)") //DEBUG
    if empty() {
      return failed()
    }
    var startOffset = offset
    var c = next()
    if c < '0' || c > '9' {
      return failed()
    }
    while true {
      if empty() {
        return succeeded(mangled[startOffset..offset])
      }
      c = next()
      if c < '0' || c > '9' {
        --offset
        return succeeded(mangled[startOffset..offset])
      }
    }
  }
}

func demangle(symbol:String) -> String {
  return Demangler(symbol).demangleSymbol().0
}

func demangleType(type:String) -> String {
  return Demangler(type).demangleType().0
}
