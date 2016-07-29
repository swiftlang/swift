// Module A

public protocol MMP1 { }
public protocol MMP2a : MMP1 { }
public protocol MMP3a : MMP2a { }
public protocol MMP2b : MMP1 { }
public protocol MMP3b : MMP2b { }
public protocol MMP4 : MMP3a, MMP3b { }

public protocol MMAnyObjectRefinement : AnyObject { }

// ---------------------------------------------------------------------------
// Define types with various conformances of various kinds
// ---------------------------------------------------------------------------

public struct MMExplicit1 : MMP1 { }

extension MMExplicit1 : MMP3a { }

open class MMSuper1 : MMP1 { }
extension MMSuper1 : MMP3a { }

open class MMSub1 : MMSuper1 { }

open class MMSuper2 : MMP1 { }
extension MMSuper2 : MMP3a { }

open class MMSub2 : MMSuper2 { }

open class MMSuper3 : MMP1 { }
open class MMSub3 : MMSuper3 { }

open class MMSuper4 : MMP1 { }

// FIXME: Enums, once the standard library deserialization is working
