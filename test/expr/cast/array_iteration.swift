// RUN: %target-parse-verify-swift

func doFoo() {}

class View {
  var subviews: Array<AnyObject>! = []
}

var rootView = View()
var v = [View(), View()]
rootView.subviews = v

rootView.subviews as! [View]

for view in rootView.subviews as! [View] {
  doFoo()
}

for view:View in rootView.subviews { // expected-error{{'AnyObject' is not convertible to 'View'}}
  doFoo()
}

(rootView.subviews!) as! [View]

(rootView.subviews) as! [View]

var ao: [AnyObject] = []
ao as! [View] // works


var b = Array<(String, Int)>()

for x in b {
  doFoo()
}

var c : Array<(String, Int)>! = Array()

for x in c {
  doFoo()
}

var d : Array<(String, Int)>? = Array()

for x in d! {
  doFoo()
}
