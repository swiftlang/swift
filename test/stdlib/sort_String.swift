// RUN: %swift %s -i | FileCheck %s

func test() {
  var array = new String[5]
  array[0] = "ysmwbgmrqlmqspzcgbgxvkebesoetqcimoaemfhfzzsamgju"
  array[1] = "nqbvcxdahvxzmvnqjmoilinznqyfniufgsfoeok"
  array[2] = "bsqdunldltkxripsaxkaojfiovnxulshiohbumirjzgfmovowiv"
  array[3] = "hepdatupehcbbdyhyjsnvtzhfupd"
  array[4] = "rlrezqqlfpoktoiitbtyngyyyeoglmwxfrxjtejmyy"
  array = sort(array)
  for i in array {
    println(i)
  }
  print('\n')
}

test()

// CHECK: bsqdunldltkxripsaxkaojfiovnxulshiohbumirjzgfmovowiv
// CHECK: hepdatupehcbbdyhyjsnvtzhfupd
// CHECK: nqbvcxdahvxzmvnqjmoilinznqyfniufgsfoeok
// CHECK: rlrezqqlfpoktoiitbtyngyyyeoglmwxfrxjtejmyy
// CHECK: ysmwbgmrqlmqspzcgbgxvkebesoetqcimoaemfhfzzsamgju
