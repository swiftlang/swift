// RUN: %target-run-simple-swift | FileCheck %s

func test() {
  var array = [String](count: 5, repeatedValue: "")
  array[0] = "ysmwbgmrqlmqspzcgbgxvkebesoetqcimoaemfhfzzsamgju"
  array[1] = "nqbvcxdahvxzmvnqjmoilinznqyfniufgsfoeok"
  array[2] = "bsqdunldltkxripsaxkaojfiovnxulshiohbumirjzgfmovowiv"
  array[3] = "hepdatupehcbbdyhyjsnvtzhfupd"
  array[4] = "rlrezqqlfpoktoiitbtyngyyyeoglmwxfrxjtejmyy"
  array = sorted(array)
  for i in array {
    print(i)
  }
  print("\n", appendNewline: false)
}

test()

// CHECK: bsqdunldltkxripsaxkaojfiovnxulshiohbumirjzgfmovowiv
// CHECK: hepdatupehcbbdyhyjsnvtzhfupd
// CHECK: nqbvcxdahvxzmvnqjmoilinznqyfniufgsfoeok
// CHECK: rlrezqqlfpoktoiitbtyngyyyeoglmwxfrxjtejmyy
// CHECK: ysmwbgmrqlmqspzcgbgxvkebesoetqcimoaemfhfzzsamgju
