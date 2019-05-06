// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | %FileCheck %s -check-prefix=AVAILABILITY1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY2 | %FileCheck %s -check-prefix=AVAILABILITY2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD2 | %FileCheck %s -check-prefix=KEYWORD2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD3 | %FileCheck %s -check-prefix=KEYWORD3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD3_2 | %FileCheck %s -check-prefix=KEYWORD3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD4 | %FileCheck %s -check-prefix=KEYWORD4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD5 | %FileCheck %s -check-prefix=KEYWORD5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD_LAST | %FileCheck %s -check-prefix=KEYWORD_LAST

@available(#^AVAILABILITY1^#)

// NOTE: Please do not include the ", N items" after "Begin completions". The
// item count creates needless merge conflicts given that we use the "-NEXT"
// feature of FileCheck and because an "End completions" line exists for each
// test.

// AVAILABILITY1: Begin completions
// AVAILABILITY1-NEXT: Keyword/None:                       *[#Platform#]; name=*{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       tvOS[#Platform#]; name=tvOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       watchOS[#Platform#]; name=watchOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       OSX[#Platform#]; name=OSX{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       iOSApplicationExtension[#Platform#]; name=iOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       tvOSApplicationExtension[#Platform#]; name=tvOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       watchOSApplicationExtension[#Platform#]; name=watchOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       OSXApplicationExtension[#Platform#]; name=OSXApplicationExtension{{$}}
// AVAILABILITY1-NEXT: End completions

@available(*, #^AVAILABILITY2^#)

// AVAILABILITY2:             Begin completions
// AVAILABILITY2-NEXT:        Keyword/None:                       unavailable; name=unavailable{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       message: [#Specify message#]; name=message{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       renamed: [#Specify replacing name#]; name=renamed{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       introduced: [#Specify version number#]; name=introduced{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       deprecated: [#Specify version number#]; name=deprecated{{$}}
// AVAILABILITY2-NEXT:        End completions

@#^KEYWORD2^#
func method(){}

// KEYWORD2:                  Begin completions
// KEYWORD2-NEXT:             Keyword/None:                       available[#Func Attribute#]; name=available{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       objc[#Func Attribute#]; name=objc{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       IBAction[#Func Attribute#]; name=IBAction{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       NSManaged[#Func Attribute#]; name=NSManaged{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       inline[#Func Attribute#]; name=inline{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       nonobjc[#Func Attribute#]; name=nonobjc{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       inlinable[#Func Attribute#]; name=inlinable{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       warn_unqualified_access[#Func Attribute#]; name=warn_unqualified_access{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       usableFromInline[#Func Attribute#]; name=usableFromInline
// KEYWORD2-NEXT:             Keyword/None:                       discardableResult[#Func Attribute#]; name=discardableResult
// SWIFT_ENABLE_TENSORFLOW
// KEYWORD2-NEXT:             Keyword/None:                       differentiable[#Func Attribute#]; name=differentiable
// KEYWORD2-NEXT:             Keyword/None:                       differentiating[#Func Attribute#]; name=differentiating
// KEYWORD2-NEXT:             Keyword/None:                       compilerEvaluable[#Func Attribute#]; name=compilerEvaluable
// KEYWORD2-NEXT:             Keyword/None:                       TensorFlowGraph[#Func Attribute#]; name=TensorFlowGraph
// KEYWORD2-NEXT:             End completions

@#^KEYWORD3^#
class C {}

// KEYWORD3:                  Begin completions
// KEYWORD3-NEXT:             Keyword/None:                       available[#Class Attribute#]; name=available{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       objc[#Class Attribute#]; name=objc{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       dynamicCallable[#Class Attribute#]; name=dynamicCallable{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       dynamicMemberLookup[#Class Attribute#]; name=dynamicMemberLookup{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       IBDesignable[#Class Attribute#]; name=IBDesignable{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       UIApplicationMain[#Class Attribute#]; name=UIApplicationMain{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       requires_stored_property_inits[#Class Attribute#]; name=requires_stored_property_inits{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       objcMembers[#Class Attribute#]; name=objcMembers{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       NSApplicationMain[#Class Attribute#]; name=NSApplicationMain{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       usableFromInline[#Class Attribute#]; name=usableFromInline
// KEYWORD3-NEXT:             Keyword/None:                       _propertyDelegate[#Class Attribute#]; name=_propertyDelegate
// KEYWORD3-NEXT:             End completions

@#^KEYWORD3_2^#IB
class C2 {}
// Same as KEYWORD3.

@#^KEYWORD4^#
enum E {}
// KEYWORD4:                  Begin completions
// KEYWORD4-NEXT:             Keyword/None:                       available[#Enum Attribute#]; name=available{{$}}
// KEYWORD4-NEXT:             Keyword/None:                       objc[#Enum Attribute#]; name=objc{{$}}
// KEYWORD4-NEXT:             Keyword/None:                       dynamicCallable[#Enum Attribute#]; name=dynamicCallable
// KEYWORD4-NEXT:             Keyword/None:                       dynamicMemberLookup[#Enum Attribute#]; name=dynamicMemberLookup
// KEYWORD4-NEXT:             Keyword/None:                       usableFromInline[#Enum Attribute#]; name=usableFromInline
// KEYWORD4-NEXT:             Keyword/None:                       _propertyDelegate[#Enum Attribute#]; name=_propertyDelegate
// KEYWORD4-NEXT:             End completions


@#^KEYWORD5^#
struct S{}
// KEYWORD5:                  Begin completions
// KEYWORD5-NEXT:             Keyword/None:                       available[#Struct Attribute#]; name=available{{$}}
// KEYWORD5-NEXT:             Keyword/None:                       dynamicCallable[#Struct Attribute#]; name=dynamicCallable
// KEYWORD5-NEXT:             Keyword/None:                       dynamicMemberLookup[#Struct Attribute#]; name=dynamicMemberLookup
// KEYWORD5-NEXT:             Keyword/None:                       usableFromInline[#Struct Attribute#]; name=usableFromInline
// KEYWORD5-NEXT:             Keyword/None:                       _propertyDelegate[#Struct Attribute#]; name=_propertyDelegate
// KEYWORD5-NEXT:             End completions


@#^KEYWORD_LAST^#

// KEYWORD_LAST:                  Begin completions
// KEYWORD_LAST-NEXT:             Keyword/None:                       available[#Declaration Attribute#]; name=available{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       objc[#Declaration Attribute#]; name=objc{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       dynamicCallable[#Declaration Attribute#]; name=dynamicCallable
// KEYWORD_LAST-NEXT:             Keyword/None:                       dynamicMemberLookup[#Declaration Attribute#]; name=dynamicMemberLookup
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSCopying[#Declaration Attribute#]; name=NSCopying{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBAction[#Declaration Attribute#]; name=IBAction{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBDesignable[#Declaration Attribute#]; name=IBDesignable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBInspectable[#Declaration Attribute#]; name=IBInspectable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBOutlet[#Declaration Attribute#]; name=IBOutlet{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSManaged[#Declaration Attribute#]; name=NSManaged{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       UIApplicationMain[#Declaration Attribute#]; name=UIApplicationMain{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       inline[#Declaration Attribute#]; name=inline{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       requires_stored_property_inits[#Declaration Attribute#]; name=requires_stored_property_inits{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       nonobjc[#Declaration Attribute#]; name=nonobjc{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       inlinable[#Declaration Attribute#]; name=inlinable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       objcMembers[#Declaration Attribute#]; name=objcMembers{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSApplicationMain[#Declaration Attribute#]; name=NSApplicationMain{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       warn_unqualified_access[#Declaration Attribute#]; name=warn_unqualified_access
// KEYWORD_LAST-NEXT:             Keyword/None:                       usableFromInline[#Declaration Attribute#]; name=usableFromInline{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       discardableResult[#Declaration Attribute#]; name=discardableResult
// KEYWORD_LAST-NEXT:             Keyword/None:                       GKInspectable[#Declaration Attribute#]; name=GKInspectable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       _propertyDelegate[#Declaration Attribute#]; name=_propertyDelegate
// SWIFT_ENABLE_TENSORFLOW
// KEYWORD_LAST-NEXT:             Keyword/None:                       differentiable[#Declaration Attribute#]; name=differentiable
// KEYWORD_LAST-NEXT:             Keyword/None:                       differentiating[#Declaration Attribute#]; name=differentiating
// KEYWORD_LAST-NEXT:             Keyword/None:                       compilerEvaluable[#Declaration Attribute#]; name=compilerEvaluable
// KEYWORD_LAST-NEXT:             Keyword/None:                       TensorFlowGraph[#Declaration Attribute#]; name=TensorFlowGraph
// KEYWORD_LAST-NEXT:             Keyword/None:                       noDerivative[#Declaration Attribute#]; name=noDerivative
// KEYWORD_LAST-NEXT:             End completions
