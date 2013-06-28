import Darwin

// FIXME: 'import Darwin' should contain this.
// rdar://14159040 Module 'Darwin' should include 'getopt.h' header
struct option {
  var name : UnsafePointer<CChar>
  var has_arg : CInt
  var flag : UnsafePointer<CInt>
  var val : CInt
}

var no_argument : CInt = 0
var required_argument : CInt = 1
var optional_argument : CInt = 2

func [asmname="getopt_long"]
    getopt_long(argc : CInt,
                argv : UnsafePointer<UnsafePointer<CChar>>,
                optstring : CString,
                longopts : UnsafePointer<option>,
                longindex : UnsafePointer<CInt>) -> CInt
// End FIXME for 'import Darwin'

struct GetoptLongOption {
  var longName : String
  var shortName : Optional<Char>

  // Contains same values as option::has_arg.  FIXME: replace with oneof.
  var hasArg : Int

  var optionPassed : Bool
  var argument : Optional<String>

  constructor(longName : String, shortName : Optional<Char>, hasArg : Int) {
    assert(shortName.isNone() || shortName.get().isASCII(),
           "short option name must be an ASCII character")

    this.longName = longName
    this.shortName = shortName
    this.hasArg = hasArg
  }
}

struct GetoptLongOptions {
  var _options : Vector<GetoptLongOption>

  constructor() {
    _options = Vector<GetoptLongOption>()
  }

  func noArgument(longName : String,
                  shortName : Optional<Char> = None) -> GetoptLongOptions {
    _options.append(GetoptLongOption(longName, shortName, Int(no_argument)))
    return this
  }

  func noArgument(longName : String,
                  shortName : Char) -> GetoptLongOptions {
    return noArgument(longName, Optional(shortName))
  }

  func requiredArgument(longName : String,
                        shortName : Optional<Char> = None)
      -> GetoptLongOptions {
    _options.append(
        GetoptLongOption(longName, shortName, Int(required_argument)))
    return this
  }

  func requiredArgument(longName : String,
                        shortName : Char) -> GetoptLongOptions {
    return requiredArgument(longName, Optional(shortName))
  }

  func optionalArgument(longName : String,
                        shortName : Optional<Char> = None)
      -> GetoptLongOptions {
    _options.append(
        GetoptLongOption(longName, shortName, Int(optional_argument)))
    return this
  }

  func optionalArgument(longName : String,
                        shortName : Char) -> GetoptLongOptions {
    return optionalArgument(longName, Optional(shortName))
  }

  func [conversion] __conversion() -> GetoptLongOption[] {
    return _options.copy().takeArray()
  }

  subscript(idx : GetoptLong.ErrorOrOption) -> GetoptLongOption {
    assert(!idx.isError, "should not have errors")
    return _options[idx.optionIndex.get()]
  }
}

class GetoptLong {
  var _LM : LifetimeManager
  var _argv : UnsafePointer<CChar>[]
  var _opts : GetoptLongOption[]
  var _optstring : CString
  var _longOptionIndex : CInt
  var _longopts : option[]
  var _shortOptionIndexes : Dictionary<Char, Int>
  var _nonOptions : Vector<String>

  constructor(argv : String[],
              opts : GetoptLongOption[],
              nonOptions : Vector<String> = Vector<String>()) {
    _LM = LifetimeManager()
    _argv = new UnsafePointer<CChar>[argv.length]
    for i in 0..argv.length {
      _argv[i] = _LM.getCString(argv[i])
    }

    _opts = opts

    _shortOptionIndexes = Dictionary<Char, Int>()

    var optstring = ":" // Return ':' instead of '?' for missing arguments.
    var longopts = Vector<option>()
    for i in 0..opts.length {
      if opts[i].longName.size() != 0 {
        longopts.append(
            option(name : _LM.getCString(opts[i].longName),
                   has_arg : CInt(opts[i].hasArg),
                   flag : UnsafePointer.addressOf(&_longOptionIndex),
                   val : CInt(i)))
      }
      if opts[i].shortName {
        optstring += opts[i].shortName.get()
        if opts[i].hasArg == Int(required_argument) {
          optstring += ":"
        } else if opts[i].hasArg == Int(optional_argument) {
          // Double colon is a GNU extension, which is apparently supported on
          // Mac OS X.
          optstring += "::"
        }
        _shortOptionIndexes[opts[i].shortName.get()] = i
      }
    }
    // Add end marker.
    longopts.append(option(UnsafePointer.null(), 0, UnsafePointer.null(), 0))

    _optstring = _LM.getCString(optstring)
    _longopts = longopts.takeArray()

    _nonOptions = nonOptions
  }

  destructor {
    _LM.release()
  }

  struct ErrorOrOption {
    var optionIndex : Optional<Int>
    var arg : Optional<String>
    var isUnknownOption : Bool
    var missingArgument : Bool

    var isError : Bool {
      return isUnknownOption || missingArgument
    }
  }

  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(this)
  }

  struct EnumeratorType : Enumerator {
    typealias Element = ErrorOrOption

    var _parent : GetoptLong
    var _nextElement : Element
    var _enumerationStarted : Bool
    var _hitEnd : Bool

    constructor(parent : GetoptLong) {
      _parent = parent
      _enumerationStarted = false
      _hitEnd = false
    }

    func _next() {
      var result = getopt_long(CInt(_parent._argv.length),
                               _parent._argv.cAddress(),
                               _parent._optstring,
                               _parent._longopts.cAddress(),
                               UnsafePointer.null())
      if result == -1 {
        _hitEnd = true

        // Collect non-options into _parent._nonOptions.
        for i in min(_parent._argv.length, Int(optind)).._parent._argv.length {
          _parent._nonOptions.append(String.fromCString(_parent._argv[i]))
        }
        return
      }
      if result == CInt(UInt8(':')) {
        _nextElement = ErrorOrOption(optionIndex : None,
                                     arg : None,
                                     isUnknownOption : false,
                                     missingArgument : true)
        return
      }
      if result == CInt(UInt8('?')) {
        _nextElement = ErrorOrOption(optionIndex : None,
                                     arg : None,
                                     isUnknownOption : true,
                                     missingArgument : false)
        return
      }
      var optionIndex : Int
      if result == 0 {
        optionIndex = Int(_parent._longOptionIndex)
      } else {
        optionIndex = _parent._shortOptionIndexes[Char(Int(result))]
      }
      var arg : Optional<String> = None
      var missingArgument = false
      if _parent._opts[optionIndex].hasArg != Int(no_argument) {
        if !optarg.isNull() {
          arg = Optional(String.fromCString(optarg))
        } else if _parent._opts[optionIndex].hasArg == Int(required_argument) {
          missingArgument = true
        }
      }
      _nextElement = ErrorOrOption(optionIndex : Optional(optionIndex),
                                   arg : arg,
                                   isUnknownOption : false,
                                   missingArgument : missingArgument)
    }

    func isEmpty() -> Bool {
      if !_enumerationStarted {
        assert(!_hitEnd, "can not hit end without starting enumeration")
        _enumerationStarted = true
        opterr = 0
        optind = 1
        optreset = 1
        _next()
      }
      return _hitEnd
    }

    func next() -> Element {
      assert(!isEmpty(), "!isEmpty()")
      var result = _nextElement
      _next()
      return result
    }
  }
}

