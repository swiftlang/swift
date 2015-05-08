// This is an OS kernel simulator, originally written in BCPL.
//
// Author:  M. J. Jordan  Cambridge Computer Laboratory.
//
// Modified by:  M. Richards, Nov 1996
//   to be ANSI C and runnable on 64 bit machines + other minor changes
// Modified by:  M. Richards, 20 Oct 1998
//   made minor corrections to improve ANSI compliance (suggested
//   by David Levine)
//
// The swift implementation is bizarre mix of rigid system design and
// powerful language features. Strong compiler optimization have been
// shown to be essential on this benchmark for JavaScript. This
// benchmark was also chosen to evaluate a certain style of enum usage
// and optional operations. Initially we mimick the C implementation
// for a fair comparison. I envision multiple versions of this
// benchmark with progressively more advanced design.
//
// TODO: Gradually make this more swift-esque as we fix performance.
// Create Task subclasses.
// Convert the function pointers into dynamic dispatch on task subtype.
// Use enum option sets when they become available.

let BufSize = 4

class Packet {
  enum Kind {
    case Dev
    case Work
  }

  var link: Packet?
  var id: Int
  var kind: Kind
  var a1: UInt32 = 0
  var a2: [UInt8] = [UInt8](count:BufSize, repeatedValue:UInt8(0))

  // This stands in for the original pkt() function.
  init(link: Packet?, id: Int, kind: Kind) {
    self.link = link
    self.id = id
    self.kind = kind
  }

  func dump() {
    print("PACKET")
    if link {
      print("link \(link!.id)")
    }
    print("id \(id)")
    print("a1 \(a1)")
    print("a2 ")
    for a in a2 {
      print("\(a) ")
    }
    print()
  }
}

// Task State Bitmask
let TSPktBit:UInt8     = 1
let TSWaitBit:UInt8    = 2
let TSHoldBit:UInt8    = 4
let TSNotHoldBit:UInt8 = 0xfb
let TSRun:UInt8         = 0
let TSRunPkt:UInt8      = 1
let TSWait:UInt8        = 2
let TSWaitPkt:UInt8     = 3
let TSHold:UInt8        = 4
let TSHoldPkt:UInt8     = 5
let TSHoldWait:UInt8    = 6
let TSHoldWaitPkt:UInt8 = 7

// Indices into a task table.
let TIIdle = 1
let TIWork = 2
let TIHandlerA = 3
let TIHandlerB = 4
let TIDevA = 5
let TIDevB = 6

class Task {
  // Bitmask
  enum State {
    case Bits(UInt8)
    var bits: UInt8 {
      switch self {
      case .Bits(var i):
        return i
      }
    }
  }
  enum Val1 {
    case None
    case TaskID(Int)
    case Worklist(Packet)

    var taskid: Int {
      get {
        switch self {
        case TaskID(var i):
          return i
        default:
          fatalError("Task value is not a task ID")
          return 0
        }
      }
      set(newid) {
        self = TaskID(newid)
      }
    }
    var packet: Packet? {
      get {
        switch self {
        case Worklist(var wkq):
          return wkq
        case None:
          return .None
        default:
          fatalError("Task value is not a worklist")
          return .None
        }
      }
      set(newpacket) {
        self = !newpacket ? None : Worklist(newpacket!)
      }
    }
  }
  // Instead of an enum, Val2 could be a field of the task subclass.
  enum Val2 {
    case None
    case Count(Int) // number of times this task should be scheduled.
    case Worklist(Packet)

    var count: Int {
      get {
        switch self {
        case Count(var i):
          return i
        default:
          fatalError("Task value is not a count")
          return 0
        }
      }
      set(newcount) {
        self = Count(newcount)
      }
    }
    var packet: Packet? {
      get {
        switch self {
        case Worklist(var wkq):
          return wkq
        case None:
          return .None
        default:
          fatalError("Task value is not a worklist")
          return .None
        }
      }
      set(newpacket) {
        self = !newpacket ? None : Worklist(newpacket!)
      }
    }
  }
  var link: Task?
  var id: Int // Array index
  var pri: Int32
  var wkq: Packet?
  var state: State
  var fn: (Packet?) -> Task?
  var v1: Val1 = .None
  var v2: Val2 = .None

  init(link:Task?, id:Int, pri:Int32, wkq:Packet?, state:State,
    fn:(Packet?) -> Task?, v1:Val1, v2:Val2) {
    self.link=link
    self.id=id
    self.pri=pri
    self.wkq=wkq
    self.state=state
    self.fn=fn
    self.v1=v1
    self.v2=v2;
  }
}

struct Richards {
  var tasktab: [Task?] = [Task?](count:11, repeatedValue:nil)
  var tasklist: Task? = .None

  // We could have each type of Task know how to create itself given
  // an id, priority, and queue. Then have a function that merely adds
  // the task to the system. For now, we intentionally mimick the C
  // implementation which passes through a large param list.
  mutating func createTask(
    id: Int,
    pri: Int32,
    wkq: Packet?,
    state: Task.State,
    fn: (Packet?) -> Task?,
    v1: Task.Val1,
    v2: Task.Val2) {

    var newtask = Task(
      link:tasklist,
      id:id,
      pri:pri,
      wkq:wkq,
      state:state,
      fn:fn,
      v1:v1,
      v2:v2)

    tasktab[id] = newtask
    tasklist = newtask
  }

  var layout = 0

  mutating func trace(a: UnicodeScalar) {
    if --layout <= 0 {
      print()
      layout = 50;
    }
    print("\(a)");
  }

  var tcb: Task? = .None
  var taskid: Int? = .None
  var v1: Task.Val1 = .None
  var v2: Task.Val2 = .None
  var tracing = false

  mutating func schedule() {
    while tcb {
      debug("TCB \(tcb!.id) state \(tcb!.state.bits)")

      var pkt:Packet? = .None
      var newtcb: Task? = .None

      switch tcb!.state {
      case .Bits(TSWaitPkt):
        pkt = tcb!.wkq
        tcb!.wkq = pkt!.link
        tcb!.state = Task.State.Bits(!tcb!.wkq ? TSRun : TSRunPkt)
        fallthrough
      case .Bits(TSRun),
        .Bits(TSRunPkt):
        taskid = tcb!.id
        v1 = tcb!.v1
        v2 = tcb!.v2
        if tracing {
          trace(UnicodeScalar(UInt32(taskid!)+"0".value))
        }
        newtcb = tcb!.fn(pkt)
        tcb!.v1 = v1
        tcb!.v2 = v2
        tcb = newtcb
        // break
      case .Bits(TSWait),
        .Bits(TSHold),
        .Bits(TSHoldPkt),
        .Bits(TSHoldWait),
        .Bits(TSHoldWaitPkt):
        tcb = tcb!.link;
      default:
        return
      }
    }
  }

  func Wait() -> Task {
    tcb!.state = Task.State.Bits(tcb!.state.bits | TSWaitBit)
    return tcb!
  }

  var holdcount = 0

  mutating func holdself() -> Task? {
    ++holdcount
    tcb!.state = Task.State.Bits(tcb!.state.bits | TSHoldBit)
    return tcb!.link
  }

  func findtcb(id: Int) -> Task? {
    var t: Task? = .None
    if 1 <= id && id <= 10 {
      t = tasktab[id]
    }
    if !t {
      print()
      print("Bad task id \(id)")
    }
    return t
  }

  func release(id: Int) -> Task? {
    var t = findtcb(id)
    if !t {
      return t
    }
    t!.state = Task.State.Bits(t!.state.bits & TSNotHoldBit)
    if t!.pri > tcb!.pri {
      return t
    }
    return tcb!
  }

  var qpktcount = 0

  mutating func qpkt(pkt: Packet) -> Task? {
    let tt = findtcb(pkt.id)
    if !tt {
      return tt
    }
    let t = tt!
    ++qpktcount
    pkt.link = .None
    pkt.id = taskid!
    if !t.wkq {
      t.wkq = pkt
      t.state = Task.State.Bits(t.state.bits | TSPktBit)
      if t.pri > tcb!.pri {
        return t
      }
    }
    else {
      append(pkt, ptr: t.wkq)
    }
    return tcb
  }

  // This could be a method on subclass IdleTask.
  // For IdleTask v2 is a count
  mutating func idlefn(pkt: Packet?) -> Task? {
    debug("IDLE")
    --v2.count
    if v2.count == 0 {
      return holdself()
    }
    // Orignal C impl masks with MAXINT. Why?
    if (v1.taskid & 1) == 0 {
      v1 = Task.Val1.TaskID(v1.taskid >> 1)
      return release(TIDevA)
    }
    else {
      v1 = Task.Val1.TaskID((v1.taskid >> 1) ^ 0xD008)
      return release(TIDevB)
    }
  }

  let alphabet: [UInt8] = Array("0ABCDEFGHIJKLMNOPQRSTUVWXYZ".utf8)

  mutating func workfn(pkt: Packet?) -> Task? {
    debug("WORK")
    if !pkt {
      return Wait()
    }
    v1 = Task.Val1.TaskID(TIHandlerA + TIHandlerB - v1.taskid)
    pkt!.id = v1.taskid
    pkt!.a1 = 0
    switch v2 {
    case .None:
      v2 = Task.Val2.Count(0)
    case .Count:
      break
    default:
      fatalError("Task value is not a count")
    }
    for i in 0..<BufSize {
      ++v2.count
      if v2.count > 26 {
        v2.count = 1
      }
      pkt!.a2[i] = alphabet[v2.count]
    }
    return qpkt(pkt!)
  }

  mutating func handlerfn(pkt: Packet?) -> Task? {
    debug("HANDLE")
    if pkt {
      debug({pkt!.dump()})
      switch pkt!.kind {
      case .Work:
        v1.packet = append(pkt!, ptr:v1.packet)
      default:
        v2.packet = append(pkt!, ptr:v2.packet)
      }
    }
    if v1.packet {
      let workpkt = v1.packet!
      let count = Int(workpkt.a1)
      if count >= BufSize {
        v1.packet = workpkt.link
        return qpkt(workpkt)
      }
      if v2.packet {
        var devpkt = v2.packet!
        v2.packet = v2.packet!.link
        devpkt.a1 = UInt32(v1.packet!.a2[count])
        v1.packet!.a1 = UInt32(count + 1)
        return qpkt(devpkt)
      }
    }
    return Wait()
  }

  mutating func devfn(pkt: Packet?) -> Task? {
    debug("DEV")
    if !pkt {
      if !v1.packet {
        return Wait()
      }
      var pkt = v1.packet
      v1.packet = .None
      return qpkt(pkt!)
    }
    v1.packet = pkt!
    if tracing {
      trace(UnicodeScalar(pkt!.a1))
    }
    return holdself()
  }

  // The C benchmark does some horrible type punning here by passing
  // the address of the queue (assuming the packet's link is the first
  // field at offset 0). Instead, we return the new queue.
  func append(pkt: Packet, ptr: Packet?) -> Packet {
    pkt.link = .None
    if !ptr {
      return pkt
    }
    var next = ptr!
    while next.link {
      next = next.link!
    }
    next.link = pkt
    return ptr!
  }

  mutating func main() -> Int {
    // Small problem size.
    let Count = 1000*1000
    let Qpktcountval = 2326410
    let Holdcountval = 930563

    print("Bench mark starting")
    var wkq: Packet? = .None
    createTask(TIIdle, pri:0, wkq:wkq, state:Task.State.Bits(TSRun), fn:idlefn,
      v1:Task.Val1.TaskID(1), v2:Task.Val2.Count(Count))

    wkq = Packet(link:.None, id:0, kind:Packet.Kind.Work)
    wkq = Packet(link:wkq, id:0, kind:Packet.Kind.Work)

    createTask(TIWork, pri:1000, wkq:wkq, state:Task.State.Bits(TSWaitPkt),
      fn:workfn, v1:Task.Val1.TaskID(TIHandlerA), v2:Task.Val2.None)

    wkq = Packet(link:.None, id:TIDevA, kind:Packet.Kind.Dev)
    wkq = Packet(link:wkq, id:TIDevA, kind:Packet.Kind.Dev)
    wkq = Packet(link:wkq, id:TIDevA, kind:Packet.Kind.Dev)

    createTask(TIHandlerA, pri:2000, wkq:wkq, state:Task.State.Bits(TSWaitPkt),
      fn:handlerfn, v1:Task.Val1.None, v2:Task.Val2.None)

    wkq = Packet(link:.None, id:TIDevB, kind:Packet.Kind.Dev)
    wkq = Packet(link:wkq, id:TIDevB, kind:Packet.Kind.Dev)
    wkq = Packet(link:wkq, id:TIDevB, kind:Packet.Kind.Dev)

    createTask(TIHandlerB, pri:3000, wkq:wkq, state:Task.State.Bits(TSWaitPkt),
      fn:handlerfn, v1:Task.Val1.None, v2:Task.Val2.None)

    wkq = .None

    createTask(TIDevA, pri:4000, wkq:wkq, state:Task.State.Bits(TSWait),
      fn:devfn, v1:Task.Val1.None, v2:Task.Val2.None)
    createTask(TIDevB, pri:5000, wkq:wkq, state:Task.State.Bits(TSWait),
      fn:devfn, v1:Task.Val1.None, v2:Task.Val2.None)

    tcb = tasklist

    print("Starting")

    schedule()

    print("Finished")

    print("qpkt count = \(qpktcount) holdcount = \(holdcount)")

    print("These results are ")
    var retval:Int
    if qpktcount == Qpktcountval && holdcount == Holdcountval {
      print("correct")
      retval = 0
    }
    else {
      print("incorrect")
      retval = 1
    }
    print("end of run")
    return retval
  }

  func debug(m:String) {}
  func debug(block: () -> ()) {}
}

var r = Richards()
r.main()
