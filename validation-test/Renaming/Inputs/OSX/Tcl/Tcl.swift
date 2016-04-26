
var TCL_ALPHA_RELEASE: Int32 { get }
var TCL_BETA_RELEASE: Int32 { get }
var TCL_FINAL_RELEASE: Int32 { get }
var TCL_MAJOR_VERSION: Int32 { get }
var TCL_MINOR_VERSION: Int32 { get }
var TCL_RELEASE_LEVEL: Int32 { get }
var TCL_RELEASE_SERIAL: Int32 { get }
var TCL_VERSION: String { get }
var TCL_PATCH_LEVEL: String { get }
typealias ClientData = UnsafeMutablePointer<Void>
var TCL_WIDE_INT_IS_LONG: Int32 { get }
var TCL_CFG_DO64BIT: Int32 { get }
typealias Tcl_WideInt = Int
typealias Tcl_WideUInt = UInt
typealias Tcl_StatBuf = stat
var TCL_LL_MODIFIER: String { get }
struct Tcl_Interp {
  var result: UnsafeMutablePointer<Int8>!
  var freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!
  var errorLine: Int32
  init()
  init(result result: UnsafeMutablePointer<Int8>!, freeProc freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!, errorLine errorLine: Int32)
}
typealias Tcl_AsyncHandler = OpaquePointer
typealias Tcl_Channel = OpaquePointer
typealias Tcl_ChannelTypeVersion = OpaquePointer
typealias Tcl_Command = OpaquePointer
typealias Tcl_Condition = OpaquePointer
typealias Tcl_Dict = OpaquePointer
typealias Tcl_EncodingState = OpaquePointer
typealias Tcl_Encoding = OpaquePointer
typealias Tcl_InterpState = OpaquePointer
typealias Tcl_LoadHandle = OpaquePointer
typealias Tcl_Mutex = OpaquePointer
typealias Tcl_Pid = OpaquePointer
typealias Tcl_RegExp = OpaquePointer
typealias Tcl_ThreadDataKey = OpaquePointer
typealias Tcl_ThreadId = OpaquePointer
typealias Tcl_TimerToken = OpaquePointer
typealias Tcl_Trace = OpaquePointer
typealias Tcl_Var = OpaquePointer
typealias Tcl_ThreadCreateProc = ((ClientData!) -> Void)
var TCL_THREAD_STACK_DEFAULT: Int32 { get }
var TCL_THREAD_NOFLAGS: Int32 { get }
var TCL_THREAD_JOINABLE: Int32 { get }
var TCL_MATCH_NOCASE: Int32 { get }
var TCL_REG_BASIC: Int32 { get }
var TCL_REG_EXTENDED: Int32 { get }
var TCL_REG_ADVF: Int32 { get }
var TCL_REG_ADVANCED: Int32 { get }
var TCL_REG_QUOTE: Int32 { get }
var TCL_REG_NOCASE: Int32 { get }
var TCL_REG_NOSUB: Int32 { get }
var TCL_REG_EXPANDED: Int32 { get }
var TCL_REG_NLSTOP: Int32 { get }
var TCL_REG_NLANCH: Int32 { get }
var TCL_REG_NEWLINE: Int32 { get }
var TCL_REG_CANMATCH: Int32 { get }
var TCL_REG_NOTBOL: Int32 { get }
var TCL_REG_NOTEOL: Int32 { get }
struct Tcl_RegExpIndices {
  var start: Int
  var end: Int
  init()
  init(start start: Int, end end: Int)
}
struct Tcl_RegExpInfo {
  var nsubs: Int32
  var matches: UnsafeMutablePointer<Tcl_RegExpIndices>!
  var extendStart: Int
  var reserved: Int
  init()
  init(nsubs nsubs: Int32, matches matches: UnsafeMutablePointer<Tcl_RegExpIndices>!, extendStart extendStart: Int, reserved reserved: Int)
}
typealias Tcl_Stat_ = UnsafeMutablePointer<Tcl_StatBuf>
typealias Tcl_OldStat_ = UnsafeMutablePointer<stat>
var TCL_OK: Int32 { get }
var TCL_ERROR: Int32 { get }
var TCL_RETURN: Int32 { get }
var TCL_BREAK: Int32 { get }
var TCL_CONTINUE: Int32 { get }
var TCL_RESULT_SIZE: Int32 { get }
var TCL_SUBST_COMMANDS: Int32 { get }
var TCL_SUBST_VARIABLES: Int32 { get }
var TCL_SUBST_BACKSLASHES: Int32 { get }
var TCL_SUBST_ALL: Int32 { get }
struct Tcl_ValueType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var TCL_INT: Tcl_ValueType { get }
var TCL_DOUBLE: Tcl_ValueType { get }
var TCL_EITHER: Tcl_ValueType { get }
var TCL_WIDE_INT: Tcl_ValueType { get }
struct Tcl_Value {
  var type: Tcl_ValueType
  var intValue: Int
  var doubleValue: Double
  var wideValue: Tcl_WideInt
  init()
  init(type type: Tcl_ValueType, intValue intValue: Int, doubleValue doubleValue: Double, wideValue wideValue: Tcl_WideInt)
}
typealias Tcl_AppInitProc = ((UnsafeMutablePointer<Tcl_Interp>!) -> Int32)
typealias Tcl_AsyncProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)
typealias Tcl_ChannelProc = ((ClientData!, Int32) -> Void)
typealias Tcl_CloseProc = ((ClientData!) -> Void)
typealias Tcl_CmdDeleteProc = ((ClientData!) -> Void)
typealias Tcl_CmdProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)
typealias Tcl_CmdTraceProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)
typealias Tcl_CmdObjTraceProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, Tcl_Command!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)
typealias Tcl_CmdObjTraceDeleteProc = ((ClientData!) -> Void)
typealias Tcl_DupInternalRepProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)
typealias Tcl_EncodingConvertProc = ((ClientData!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)
typealias Tcl_EncodingFreeProc = ((ClientData!) -> Void)
typealias Tcl_EventProc = ((UnsafeMutablePointer<Tcl_Event>!, Int32) -> Int32)
typealias Tcl_EventCheckProc = ((ClientData!, Int32) -> Void)
typealias Tcl_EventDeleteProc = ((UnsafeMutablePointer<Tcl_Event>!, ClientData!) -> Int32)
typealias Tcl_EventSetupProc = ((ClientData!, Int32) -> Void)
typealias Tcl_ExitProc = ((ClientData!) -> Void)
typealias Tcl_FileProc = ((ClientData!, Int32) -> Void)
typealias Tcl_FileFreeProc = ((ClientData!) -> Void)
typealias Tcl_FreeInternalRepProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> Void)
typealias Tcl_FreeProc = ((UnsafeMutablePointer<Int8>!) -> Void)
typealias Tcl_IdleProc = ((ClientData!) -> Void)
typealias Tcl_InterpDeleteProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)
typealias Tcl_MathProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)
typealias Tcl_NamespaceDeleteProc = ((ClientData!) -> Void)
typealias Tcl_ObjCmdProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)
typealias Tcl_PackageInitProc = ((UnsafeMutablePointer<Tcl_Interp>!) -> Int32)
typealias Tcl_PackageUnloadProc = ((UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)
typealias Tcl_TcpAcceptProc = ((ClientData!, Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Void)
typealias Tcl_TimerProc = ((ClientData!) -> Void)
typealias Tcl_SetFromAnyProc = ((UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_UpdateStringProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> Void)
typealias Tcl_VarTraceProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)
typealias Tcl_CommandTraceProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)
typealias Tcl_CreateFileHandlerProc = ((Int32, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)
typealias Tcl_DeleteFileHandlerProc = ((Int32) -> Void)
typealias Tcl_AlertNotifierProc = ((ClientData!) -> Void)
typealias Tcl_ServiceModeHookProc = ((Int32) -> Void)
typealias Tcl_InitNotifierProc = (() -> ClientData!)
typealias Tcl_FinalizeNotifierProc = ((ClientData!) -> Void)
typealias Tcl_MainLoopProc = (() -> Void)
struct Tcl_ObjType {
  var name: UnsafeMutablePointer<Int8>!
  var freeIntRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var dupIntRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var updateStringProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var setFromAnyProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  init()
  init(name name: UnsafeMutablePointer<Int8>!, freeIntRepProc freeIntRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, dupIntRepProc dupIntRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, updateStringProc updateStringProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, setFromAnyProc setFromAnyProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!)
}
struct Tcl_Obj {
  struct __Unnamed_union_internalRep {
    struct __Unnamed_struct_twoPtrValue {
      var ptr1: UnsafeMutablePointer<Void>!
      var ptr2: UnsafeMutablePointer<Void>!
      init()
      init(ptr1 ptr1: UnsafeMutablePointer<Void>!, ptr2 ptr2: UnsafeMutablePointer<Void>!)
    }
    struct __Unnamed_struct_ptrAndLongRep {
      var ptr: UnsafeMutablePointer<Void>!
      var value: UInt
      init()
      init(ptr ptr: UnsafeMutablePointer<Void>!, value value: UInt)
    }
    var longValue: Int
    var doubleValue: Double
    var otherValuePtr: UnsafeMutablePointer<Void>!
    var wideValue: Tcl_WideInt
    var twoPtrValue: Tcl_Obj.__Unnamed_union_internalRep.__Unnamed_struct_twoPtrValue
    var ptrAndLongRep: Tcl_Obj.__Unnamed_union_internalRep.__Unnamed_struct_ptrAndLongRep
    init(longValue longValue: Int)
    init(doubleValue doubleValue: Double)
    init(otherValuePtr otherValuePtr: UnsafeMutablePointer<Void>!)
    init(wideValue wideValue: Tcl_WideInt)
    init(twoPtrValue twoPtrValue: Tcl_Obj.__Unnamed_union_internalRep.__Unnamed_struct_twoPtrValue)
    init(ptrAndLongRep ptrAndLongRep: Tcl_Obj.__Unnamed_union_internalRep.__Unnamed_struct_ptrAndLongRep)
    init()
  }
  var refCount: Int32
  var bytes: UnsafeMutablePointer<Int8>!
  var length: Int32
  var typePtr: UnsafeMutablePointer<Tcl_ObjType>!
  var internalRep: Tcl_Obj.__Unnamed_union_internalRep
  init()
  init(refCount refCount: Int32, bytes bytes: UnsafeMutablePointer<Int8>!, length length: Int32, typePtr typePtr: UnsafeMutablePointer<Tcl_ObjType>!, internalRep internalRep: Tcl_Obj.__Unnamed_union_internalRep)
}
func Tcl_IncrRefCount(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_DecrRefCount(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
@discardableResult
func Tcl_IsShared(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
struct Tcl_SavedResult {
  var result: UnsafeMutablePointer<Int8>!
  var freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!
  var objResultPtr: UnsafeMutablePointer<Tcl_Obj>!
  var appendResult: UnsafeMutablePointer<Int8>!
  var appendAvl: Int32
  var appendUsed: Int32
  var resultSpace: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  init()
  init(result result: UnsafeMutablePointer<Int8>!, freeProc freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!, objResultPtr objResultPtr: UnsafeMutablePointer<Tcl_Obj>!, appendResult appendResult: UnsafeMutablePointer<Int8>!, appendAvl appendAvl: Int32, appendUsed appendUsed: Int32, resultSpace resultSpace: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8))
}
struct Tcl_Namespace {
  var name: UnsafeMutablePointer<Int8>!
  var fullName: UnsafeMutablePointer<Int8>!
  var clientData: ClientData!
  var deleteProc: (@convention(c) (ClientData!) -> Void)!
  var parentPtr: UnsafeMutablePointer<Tcl_Namespace>!
  init()
  init(name name: UnsafeMutablePointer<Int8>!, fullName fullName: UnsafeMutablePointer<Int8>!, clientData clientData: ClientData!, deleteProc deleteProc: (@convention(c) (ClientData!) -> Void)!, parentPtr parentPtr: UnsafeMutablePointer<Tcl_Namespace>!)
}
struct Tcl_CallFrame {
  var nsPtr: UnsafeMutablePointer<Tcl_Namespace>!
  var dummy1: Int32
  var dummy2: Int32
  var dummy3: UnsafeMutablePointer<Void>!
  var dummy4: UnsafeMutablePointer<Void>!
  var dummy5: UnsafeMutablePointer<Void>!
  var dummy6: Int32
  var dummy7: UnsafeMutablePointer<Void>!
  var dummy8: UnsafeMutablePointer<Void>!
  var dummy9: Int32
  var dummy10: UnsafeMutablePointer<Void>!
  var dummy11: UnsafeMutablePointer<Void>!
  var dummy12: UnsafeMutablePointer<Void>!
  var dummy13: UnsafeMutablePointer<Void>!
  init()
  init(nsPtr nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, dummy1 dummy1: Int32, dummy2 dummy2: Int32, dummy3 dummy3: UnsafeMutablePointer<Void>!, dummy4 dummy4: UnsafeMutablePointer<Void>!, dummy5 dummy5: UnsafeMutablePointer<Void>!, dummy6 dummy6: Int32, dummy7 dummy7: UnsafeMutablePointer<Void>!, dummy8 dummy8: UnsafeMutablePointer<Void>!, dummy9 dummy9: Int32, dummy10 dummy10: UnsafeMutablePointer<Void>!, dummy11 dummy11: UnsafeMutablePointer<Void>!, dummy12 dummy12: UnsafeMutablePointer<Void>!, dummy13 dummy13: UnsafeMutablePointer<Void>!)
}
struct Tcl_CmdInfo {
  var isNativeObjectProc: Int32
  var objProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var objClientData: ClientData!
  var proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!
  var clientData: ClientData!
  var deleteProc: (@convention(c) (ClientData!) -> Void)!
  var deleteData: ClientData!
  var namespacePtr: UnsafeMutablePointer<Tcl_Namespace>!
  init()
  init(isNativeObjectProc isNativeObjectProc: Int32, objProc objProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, objClientData objClientData: ClientData!, proc proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, clientData clientData: ClientData!, deleteProc deleteProc: (@convention(c) (ClientData!) -> Void)!, deleteData deleteData: ClientData!, namespacePtr namespacePtr: UnsafeMutablePointer<Tcl_Namespace>!)
}
var TCL_DSTRING_STATIC_SIZE: Int32 { get }
struct Tcl_DString {
  var string: UnsafeMutablePointer<Int8>!
  var length: Int32
  var spaceAvl: Int32
  var staticSpace: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  init()
  init(string string: UnsafeMutablePointer<Int8>!, length length: Int32, spaceAvl spaceAvl: Int32, staticSpace staticSpace: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8))
}
var TCL_MAX_PREC: Int32 { get }
var TCL_INTEGER_SPACE: Int32 { get }
var TCL_DONT_USE_BRACES: Int32 { get }
var TCL_DONT_QUOTE_HASH: Int32 { get }
var TCL_EXACT: Int32 { get }
var TCL_NO_EVAL: Int32 { get }
var TCL_EVAL_GLOBAL: Int32 { get }
var TCL_EVAL_DIRECT: Int32 { get }
var TCL_EVAL_INVOKE: Int32 { get }
var TCL_GLOBAL_ONLY: Int32 { get }
var TCL_NAMESPACE_ONLY: Int32 { get }
var TCL_APPEND_VALUE: Int32 { get }
var TCL_LIST_ELEMENT: Int32 { get }
var TCL_TRACE_READS: Int32 { get }
var TCL_TRACE_WRITES: Int32 { get }
var TCL_TRACE_UNSETS: Int32 { get }
var TCL_TRACE_DESTROYED: Int32 { get }
var TCL_INTERP_DESTROYED: Int32 { get }
var TCL_LEAVE_ERR_MSG: Int32 { get }
var TCL_TRACE_ARRAY: Int32 { get }
var TCL_TRACE_OLD_STYLE: Int32 { get }
var TCL_TRACE_RESULT_DYNAMIC: Int32 { get }
var TCL_TRACE_RESULT_OBJECT: Int32 { get }
var TCL_ENSEMBLE_PREFIX: Int32 { get }
var TCL_TRACE_RENAME: Int32 { get }
var TCL_TRACE_DELETE: Int32 { get }
var TCL_ALLOW_INLINE_COMPILATION: Int32 { get }
var TCL_PARSE_PART1: Int32 { get }
var TCL_LINK_INT: Int32 { get }
var TCL_LINK_DOUBLE: Int32 { get }
var TCL_LINK_BOOLEAN: Int32 { get }
var TCL_LINK_STRING: Int32 { get }
var TCL_LINK_WIDE_INT: Int32 { get }
var TCL_LINK_CHAR: Int32 { get }
var TCL_LINK_UCHAR: Int32 { get }
var TCL_LINK_SHORT: Int32 { get }
var TCL_LINK_USHORT: Int32 { get }
var TCL_LINK_UINT: Int32 { get }
var TCL_LINK_LONG: Int32 { get }
var TCL_LINK_ULONG: Int32 { get }
var TCL_LINK_FLOAT: Int32 { get }
var TCL_LINK_WIDE_UINT: Int32 { get }
var TCL_LINK_READ_ONLY: Int32 { get }
typealias Tcl_HashKeyProc = ((UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UInt32)
typealias Tcl_CompareHashKeysProc = ((UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Tcl_HashEntry>!) -> Int32)
typealias Tcl_AllocHashEntryProc = ((UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)
typealias Tcl_FreeHashEntryProc = ((UnsafeMutablePointer<Tcl_HashEntry>!) -> Void)
var TCL_HASH_KEY_STORE_HASH: Int32 { get }
struct Tcl_HashEntry {
  struct __Unnamed_union_key {
    var oneWordValue: UnsafeMutablePointer<Int8>!
    var objPtr: UnsafeMutablePointer<Tcl_Obj>!
    var words: (Int32)
    var string: (Int8, Int8, Int8, Int8)
    init(oneWordValue oneWordValue: UnsafeMutablePointer<Int8>!)
    init(objPtr objPtr: UnsafeMutablePointer<Tcl_Obj>!)
    init(words words: (Int32))
    init(string string: (Int8, Int8, Int8, Int8))
    init()
  }
  var nextPtr: UnsafeMutablePointer<Tcl_HashEntry>!
  var tablePtr: UnsafeMutablePointer<Tcl_HashTable>!
  var hash: UnsafeMutablePointer<Void>!
  var clientData: ClientData!
  var key: Tcl_HashEntry.__Unnamed_union_key
  init()
  init(nextPtr nextPtr: UnsafeMutablePointer<Tcl_HashEntry>!, tablePtr tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, hash hash: UnsafeMutablePointer<Void>!, clientData clientData: ClientData!, key key: Tcl_HashEntry.__Unnamed_union_key)
}
var TCL_HASH_KEY_RANDOMIZE_HASH: Int32 { get }
var TCL_HASH_KEY_SYSTEM_HASH: Int32 { get }
var TCL_HASH_KEY_TYPE_VERSION: Int32 { get }
struct Tcl_HashKeyType {
  var version: Int32
  var flags: Int32
  var hashKeyProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UInt32)!
  var compareKeysProc: (@convention(c) (UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Tcl_HashEntry>!) -> Int32)!
  var allocEntryProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var freeEntryProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashEntry>!) -> Void)!
  init()
  init(version version: Int32, flags flags: Int32, hashKeyProc hashKeyProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UInt32)!, compareKeysProc compareKeysProc: (@convention(c) (UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Tcl_HashEntry>!) -> Int32)!, allocEntryProc allocEntryProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, freeEntryProc freeEntryProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashEntry>!) -> Void)!)
}
var TCL_SMALL_HASH_TABLE: Int32 { get }
struct Tcl_HashTable {
  var buckets: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_HashEntry>?>!
  var staticBuckets: (UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?)
  var numBuckets: Int32
  var numEntries: Int32
  var rebuildSize: Int32
  var downShift: Int32
  var mask: Int32
  var keyType: Int32
  var findProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var createProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var typePtr: UnsafeMutablePointer<Tcl_HashKeyType>!
  init()
  init(buckets buckets: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_HashEntry>?>!, staticBuckets staticBuckets: (UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?, UnsafeMutablePointer<Tcl_HashEntry>?), numBuckets numBuckets: Int32, numEntries numEntries: Int32, rebuildSize rebuildSize: Int32, downShift downShift: Int32, mask mask: Int32, keyType keyType: Int32, findProc findProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, createProc createProc: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, typePtr typePtr: UnsafeMutablePointer<Tcl_HashKeyType>!)
}
struct Tcl_HashSearch {
  var tablePtr: UnsafeMutablePointer<Tcl_HashTable>!
  var nextIndex: Int32
  var nextEntryPtr: UnsafeMutablePointer<Tcl_HashEntry>!
  init()
  init(tablePtr tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, nextIndex nextIndex: Int32, nextEntryPtr nextEntryPtr: UnsafeMutablePointer<Tcl_HashEntry>!)
}
var TCL_STRING_KEYS: Int32 { get }
var TCL_ONE_WORD_KEYS: Int32 { get }
var TCL_CUSTOM_TYPE_KEYS: Int32 { get }
var TCL_CUSTOM_PTR_KEYS: Int32 { get }
struct Tcl_DictSearch {
  var next: UnsafeMutablePointer<Void>!
  var epoch: Int32
  var dictionaryPtr: Tcl_Dict!
  init()
  init(next next: UnsafeMutablePointer<Void>!, epoch epoch: Int32, dictionaryPtr dictionaryPtr: Tcl_Dict!)
}
var TCL_DONT_WAIT: Int32 { get }
var TCL_WINDOW_EVENTS: Int32 { get }
var TCL_FILE_EVENTS: Int32 { get }
var TCL_TIMER_EVENTS: Int32 { get }
var TCL_IDLE_EVENTS: Int32 { get }
struct Tcl_Event {
  var proc: (@convention(c) (UnsafeMutablePointer<Tcl_Event>!, Int32) -> Int32)!
  var nextPtr: UnsafeMutablePointer<Tcl_Event>!
  init()
  init(proc proc: (@convention(c) (UnsafeMutablePointer<Tcl_Event>!, Int32) -> Int32)!, nextPtr nextPtr: UnsafeMutablePointer<Tcl_Event>!)
}
struct Tcl_QueuePosition : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var TCL_QUEUE_TAIL: Tcl_QueuePosition { get }
var TCL_QUEUE_HEAD: Tcl_QueuePosition { get }
var TCL_QUEUE_MARK: Tcl_QueuePosition { get }
var TCL_SERVICE_NONE: Int32 { get }
var TCL_SERVICE_ALL: Int32 { get }
struct Tcl_Time {
  var sec: Int
  var usec: Int
  init()
  init(sec sec: Int, usec usec: Int)
}
typealias Tcl_SetTimerProc = ((UnsafeMutablePointer<Tcl_Time>!) -> Void)
typealias Tcl_WaitForEventProc = ((UnsafeMutablePointer<Tcl_Time>!) -> Int32)
typealias Tcl_GetTimeProc = ((UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)
typealias Tcl_ScaleTimeProc = ((UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)
var TCL_READABLE: Int32 { get }
var TCL_WRITABLE: Int32 { get }
var TCL_EXCEPTION: Int32 { get }
var TCL_STDIN: Int32 { get }
var TCL_STDOUT: Int32 { get }
var TCL_STDERR: Int32 { get }
var TCL_ENFORCE_MODE: Int32 { get }
var TCL_CLOSE_READ: Int32 { get }
var TCL_CLOSE_WRITE: Int32 { get }
var TCL_CHANNEL_THREAD_INSERT: Int32 { get }
var TCL_CHANNEL_THREAD_REMOVE: Int32 { get }
typealias Tcl_DriverBlockModeProc = ((ClientData!, Int32) -> Int32)
typealias Tcl_DriverCloseProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)
typealias Tcl_DriverClose2Proc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)
typealias Tcl_DriverInputProc = ((ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)
typealias Tcl_DriverOutputProc = ((ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)
typealias Tcl_DriverSeekProc = ((ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)
typealias Tcl_DriverSetOptionProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)
typealias Tcl_DriverGetOptionProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)
typealias Tcl_DriverWatchProc = ((ClientData!, Int32) -> Void)
typealias Tcl_DriverGetHandleProc = ((ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)
typealias Tcl_DriverFlushProc = ((ClientData!) -> Int32)
typealias Tcl_DriverHandlerProc = ((ClientData!, Int32) -> Int32)
typealias Tcl_DriverWideSeekProc = ((ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)
typealias Tcl_DriverThreadActionProc = ((ClientData!, Int32) -> Void)
typealias Tcl_DriverTruncateProc = ((ClientData!, Tcl_WideInt) -> Int32)
struct Tcl_ChannelType {
  var typeName: UnsafeMutablePointer<Int8>!
  var version: Tcl_ChannelTypeVersion!
  var closeProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var inputProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var outputProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var seekProc: (@convention(c) (ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var setOptionProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var getOptionProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!
  var watchProc: (@convention(c) (ClientData!, Int32) -> Void)!
  var getHandleProc: (@convention(c) (ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var close2Proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var blockModeProc: (@convention(c) (ClientData!, Int32) -> Int32)!
  var flushProc: (@convention(c) (ClientData!) -> Int32)!
  var handlerProc: (@convention(c) (ClientData!, Int32) -> Int32)!
  var wideSeekProc: (@convention(c) (ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)!
  var threadActionProc: (@convention(c) (ClientData!, Int32) -> Void)!
  var truncateProc: (@convention(c) (ClientData!, Tcl_WideInt) -> Int32)!
  init()
  init(typeName typeName: UnsafeMutablePointer<Int8>!, version version: Tcl_ChannelTypeVersion!, closeProc closeProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, inputProc inputProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, outputProc outputProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, seekProc seekProc: (@convention(c) (ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, setOptionProc setOptionProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, getOptionProc getOptionProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!, watchProc watchProc: (@convention(c) (ClientData!, Int32) -> Void)!, getHandleProc getHandleProc: (@convention(c) (ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!, close2Proc close2Proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, blockModeProc blockModeProc: (@convention(c) (ClientData!, Int32) -> Int32)!, flushProc flushProc: (@convention(c) (ClientData!) -> Int32)!, handlerProc handlerProc: (@convention(c) (ClientData!, Int32) -> Int32)!, wideSeekProc wideSeekProc: (@convention(c) (ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)!, threadActionProc threadActionProc: (@convention(c) (ClientData!, Int32) -> Void)!, truncateProc truncateProc: (@convention(c) (ClientData!, Tcl_WideInt) -> Int32)!)
}
var TCL_MODE_BLOCKING: Int32 { get }
var TCL_MODE_NONBLOCKING: Int32 { get }
struct Tcl_PathType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var TCL_PATH_ABSOLUTE: Tcl_PathType { get }
var TCL_PATH_RELATIVE: Tcl_PathType { get }
var TCL_PATH_VOLUME_RELATIVE: Tcl_PathType { get }
struct Tcl_GlobTypeData {
  var type: Int32
  var perm: Int32
  var macType: UnsafeMutablePointer<Tcl_Obj>!
  var macCreator: UnsafeMutablePointer<Tcl_Obj>!
  init()
  init(type type: Int32, perm perm: Int32, macType macType: UnsafeMutablePointer<Tcl_Obj>!, macCreator macCreator: UnsafeMutablePointer<Tcl_Obj>!)
}
var TCL_GLOB_TYPE_BLOCK: Int32 { get }
var TCL_GLOB_TYPE_CHAR: Int32 { get }
var TCL_GLOB_TYPE_DIR: Int32 { get }
var TCL_GLOB_TYPE_PIPE: Int32 { get }
var TCL_GLOB_TYPE_FILE: Int32 { get }
var TCL_GLOB_TYPE_LINK: Int32 { get }
var TCL_GLOB_TYPE_SOCK: Int32 { get }
var TCL_GLOB_TYPE_MOUNT: Int32 { get }
var TCL_GLOB_PERM_RONLY: Int32 { get }
var TCL_GLOB_PERM_HIDDEN: Int32 { get }
var TCL_GLOB_PERM_R: Int32 { get }
var TCL_GLOB_PERM_W: Int32 { get }
var TCL_GLOB_PERM_X: Int32 { get }
var TCL_UNLOAD_DETACH_FROM_INTERPRETER: Int32 { get }
var TCL_UNLOAD_DETACH_FROM_PROCESS: Int32 { get }
typealias Tcl_FSStatProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)
typealias Tcl_FSAccessProc = ((UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)
typealias Tcl_FSOpenFileChannelProc = ((UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> Tcl_Channel!)
typealias Tcl_FSMatchInDirectoryProc = ((UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32)
typealias Tcl_FSGetCwdProc = ((UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSChdirProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSLstatProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)
typealias Tcl_FSCreateDirectoryProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSDeleteFileProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSCopyDirectoryProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)
typealias Tcl_FSCopyFileProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSRemoveDirectoryProc = ((UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)
typealias Tcl_FSRenameFileProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSUnloadFileProc = ((Tcl_LoadHandle!) -> Void)
typealias Tcl_FSListVolumesProc = (() -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSUtimeProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<utimbuf>!) -> Int32)
typealias Tcl_FSNormalizePathProc = ((UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)
typealias Tcl_FSFileAttrsGetProc = ((UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)
typealias Tcl_FSFileAttrStringsProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!)
typealias Tcl_FSFileAttrsSetProc = ((UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)
typealias Tcl_FSLinkProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSLoadFileProc = ((UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_LoadHandle?>!, UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32)
typealias Tcl_FSPathInFilesystemProc = ((UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<ClientData?>!) -> Int32)
typealias Tcl_FSFilesystemPathTypeProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSFilesystemSeparatorProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSFreeInternalRepProc = ((ClientData!) -> Void)
typealias Tcl_FSDupInternalRepProc = ((ClientData!) -> ClientData!)
typealias Tcl_FSInternalToNormalizedProc = ((ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!)
typealias Tcl_FSCreateInternalRepProc = ((UnsafeMutablePointer<Tcl_Obj>!) -> ClientData!)
typealias Tcl_FSVersion = OpaquePointer
struct Tcl_Filesystem {
  var typeName: UnsafePointer<Int8>!
  var structureLength: Int32
  var version: Tcl_FSVersion!
  var pathInFilesystemProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var dupInternalRepProc: (@convention(c) (ClientData!) -> ClientData!)!
  var freeInternalRepProc: (@convention(c) (ClientData!) -> Void)!
  var internalToNormalizedProc: (@convention(c) (ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var createInternalRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> ClientData!)!
  var normalizePathProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var filesystemPathTypeProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var filesystemSeparatorProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var statProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!
  var accessProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var openFileChannelProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> Tcl_Channel!)!
  var matchInDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32)!
  var utimeProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<utimbuf>!) -> Int32)!
  var linkProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var listVolumesProc: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!
  var fileAttrStringsProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!)!
  var fileAttrsGetProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var fileAttrsSetProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var createDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var removeDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var deleteFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var copyFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var renameFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var copyDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var lstatProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!
  var loadFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_LoadHandle?>!, UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32)!
  var getCwdProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var chdirProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  init()
  init(typeName typeName: UnsafePointer<Int8>!, structureLength structureLength: Int32, version version: Tcl_FSVersion!, pathInFilesystemProc pathInFilesystemProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!, dupInternalRepProc dupInternalRepProc: (@convention(c) (ClientData!) -> ClientData!)!, freeInternalRepProc freeInternalRepProc: (@convention(c) (ClientData!) -> Void)!, internalToNormalizedProc internalToNormalizedProc: (@convention(c) (ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!)!, createInternalRepProc createInternalRepProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> ClientData!)!, normalizePathProc normalizePathProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, filesystemPathTypeProc filesystemPathTypeProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, filesystemSeparatorProc filesystemSeparatorProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, statProc statProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!, accessProc accessProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, openFileChannelProc openFileChannelProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> Tcl_Channel!)!, matchInDirectoryProc matchInDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32)!, utimeProc utimeProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<utimbuf>!) -> Int32)!, linkProc linkProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, listVolumesProc listVolumesProc: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!, fileAttrStringsProc fileAttrStringsProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!)!, fileAttrsGetProc fileAttrsGetProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, fileAttrsSetProc fileAttrsSetProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, createDirectoryProc createDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, removeDirectoryProc removeDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, deleteFileProc deleteFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, copyFileProc copyFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, renameFileProc renameFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, copyDirectoryProc copyDirectoryProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, lstatProc lstatProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!, loadFileProc loadFileProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_LoadHandle?>!, UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32)!, getCwdProc getCwdProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, chdirProc chdirProc: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!)
}
var TCL_CREATE_SYMBOLIC_LINK: Int32 { get }
var TCL_CREATE_HARD_LINK: Int32 { get }
struct Tcl_NotifierProcs {
  var setTimerProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var waitForEventProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Int32)!
  var createFileHandlerProc: (@convention(c) (Int32, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var deleteFileHandlerProc: (@convention(c) (Int32) -> Void)!
  var initNotifierProc: (@convention(c) () -> ClientData!)!
  var finalizeNotifierProc: (@convention(c) (ClientData!) -> Void)!
  var alertNotifierProc: (@convention(c) (ClientData!) -> Void)!
  var serviceModeHookProc: (@convention(c) (Int32) -> Void)!
  init()
  init(setTimerProc setTimerProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!, waitForEventProc waitForEventProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Int32)!, createFileHandlerProc createFileHandlerProc: (@convention(c) (Int32, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, deleteFileHandlerProc deleteFileHandlerProc: (@convention(c) (Int32) -> Void)!, initNotifierProc initNotifierProc: (@convention(c) () -> ClientData!)!, finalizeNotifierProc finalizeNotifierProc: (@convention(c) (ClientData!) -> Void)!, alertNotifierProc alertNotifierProc: (@convention(c) (ClientData!) -> Void)!, serviceModeHookProc serviceModeHookProc: (@convention(c) (Int32) -> Void)!)
}
struct Tcl_EncodingType {
  var encodingName: UnsafePointer<Int8>!
  var toUtfProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var fromUtfProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var freeProc: (@convention(c) (ClientData!) -> Void)!
  var clientData: ClientData!
  var nullSize: Int32
  init()
  init(encodingName encodingName: UnsafePointer<Int8>!, toUtfProc toUtfProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!, fromUtfProc fromUtfProc: (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!, freeProc freeProc: (@convention(c) (ClientData!) -> Void)!, clientData clientData: ClientData!, nullSize nullSize: Int32)
}
var TCL_ENCODING_START: Int32 { get }
var TCL_ENCODING_END: Int32 { get }
var TCL_ENCODING_STOPONERROR: Int32 { get }
struct Tcl_Token {
  var type: Int32
  var start: UnsafePointer<Int8>!
  var size: Int32
  var numComponents: Int32
  init()
  init(type type: Int32, start start: UnsafePointer<Int8>!, size size: Int32, numComponents numComponents: Int32)
}
var TCL_TOKEN_WORD: Int32 { get }
var TCL_TOKEN_SIMPLE_WORD: Int32 { get }
var TCL_TOKEN_TEXT: Int32 { get }
var TCL_TOKEN_BS: Int32 { get }
var TCL_TOKEN_COMMAND: Int32 { get }
var TCL_TOKEN_VARIABLE: Int32 { get }
var TCL_TOKEN_SUB_EXPR: Int32 { get }
var TCL_TOKEN_OPERATOR: Int32 { get }
var TCL_TOKEN_EXPAND_WORD: Int32 { get }
var TCL_PARSE_SUCCESS: Int32 { get }
var TCL_PARSE_QUOTE_EXTRA: Int32 { get }
var TCL_PARSE_BRACE_EXTRA: Int32 { get }
var TCL_PARSE_MISSING_BRACE: Int32 { get }
var TCL_PARSE_MISSING_BRACKET: Int32 { get }
var TCL_PARSE_MISSING_PAREN: Int32 { get }
var TCL_PARSE_MISSING_QUOTE: Int32 { get }
var TCL_PARSE_MISSING_VAR_BRACE: Int32 { get }
var TCL_PARSE_SYNTAX: Int32 { get }
var TCL_PARSE_BAD_NUMBER: Int32 { get }
var NUM_STATIC_TOKENS: Int32 { get }
struct Tcl_Parse {
  var commentStart: UnsafePointer<Int8>!
  var commentSize: Int32
  var commandStart: UnsafePointer<Int8>!
  var commandSize: Int32
  var numWords: Int32
  var tokenPtr: UnsafeMutablePointer<Tcl_Token>!
  var numTokens: Int32
  var tokensAvailable: Int32
  var errorType: Int32
  var string: UnsafePointer<Int8>!
  var end: UnsafePointer<Int8>!
  var interp: UnsafeMutablePointer<Tcl_Interp>!
  var term: UnsafePointer<Int8>!
  var incomplete: Int32
  var staticTokens: (Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token)
  init()
  init(commentStart commentStart: UnsafePointer<Int8>!, commentSize commentSize: Int32, commandStart commandStart: UnsafePointer<Int8>!, commandSize commandSize: Int32, numWords numWords: Int32, tokenPtr tokenPtr: UnsafeMutablePointer<Tcl_Token>!, numTokens numTokens: Int32, tokensAvailable tokensAvailable: Int32, errorType errorType: Int32, string string: UnsafePointer<Int8>!, end end: UnsafePointer<Int8>!, interp interp: UnsafeMutablePointer<Tcl_Interp>!, term term: UnsafePointer<Int8>!, incomplete incomplete: Int32, staticTokens staticTokens: (Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token, Tcl_Token))
}
var TCL_CONVERT_MULTIBYTE: Int32 { get }
var TCL_CONVERT_SYNTAX: Int32 { get }
var TCL_CONVERT_UNKNOWN: Int32 { get }
var TCL_CONVERT_NOSPACE: Int32 { get }
var TCL_UTF_MAX: Int32 { get }
typealias Tcl_UniChar = UInt16
struct Tcl_Config {
  var key: UnsafePointer<Int8>!
  var value: UnsafePointer<Int8>!
  init()
  init(key key: UnsafePointer<Int8>!, value value: UnsafePointer<Int8>!)
}
var TCL_LIMIT_COMMANDS: Int32 { get }
var TCL_LIMIT_TIME: Int32 { get }
typealias Tcl_LimitHandlerProc = ((ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)
typealias Tcl_LimitHandlerDeleteProc = ((ClientData!) -> Void)
typealias mp_digit = UInt32
@discardableResult
func Tcl_InitStubs(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ version: UnsafePointer<Int8>!, _ exact: Int32) -> UnsafePointer<Int8>!
@discardableResult
func TclTomMathInitializeStubs(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ version: UnsafePointer<Int8>!, _ epoch: Int32, _ revision: Int32) -> UnsafePointer<Int8>!
func Tcl_Main(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ appInitProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!)
@discardableResult
func Tcl_PkgInitStubsCheck(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ version: UnsafePointer<Int8>!, _ exact: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_AppInit(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
