// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %S/Inputs/serialize_functions.json %t -u

func oneName(name: String) {}
func oneName(firstName secondName: String) {}
func const(_const _ map: String) {}
func isolated(isolated _ map: String) {}
