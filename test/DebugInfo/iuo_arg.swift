// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s

class CGImageRef {}
class UIImage {
  init() {}
  var CGImage : CGImageRef? {
    get { return self.CGImage }
  }
}
class NSDictionary {}
class CIFilter {
  init (name: String) {}
}

class MyClass {
  // CHECK: define %C7iuo_arg7UIImage* @_TFC7iuo_arg7MyClass11filterImagefS0_FTGSQCS_7UIImage_Sb_S1_
  func filterImage(image: UIImage!, _ doSomething:Bool) -> UIImage
	{
    // Test that image is not an indirect location.
    // CHECK: call void @llvm.dbg.value(metadata !{i64 %0}, i64 0, metadata ![[IMAGE:.*]])
    // CHECK: ![[IMAGE]] = {{.*}} i32 0} ; [ DW_TAG_arg_variable ] [image] [line [[@LINE-4]]]
		let filter = CIFilter(name: "CIGaussianBlur")
		return image
	}
}

let a = MyClass()
let img = a.filterImage(UIImage(), true)

