// RUN: %swift %s -i | FileCheck %s

func sort(array : String[])
{
  for var i = 1; i < array.length; ++i {
    var j = i
    var tmp = array[j]
    for var k = i-1; j != 0 && tmp < array[k]; --j {
      array[j] = array[k]
      --k
    }
    array[j] = tmp
  }
}

func test() {
  var array = new String[5]
  array[0] = "ysmwbgmrqlmqspzcgbgxvkebesoetqcimoaemfhfzzsamgju"
  array[1] = "nqbvcxdahvxzmvnqjmoilinznqyfniufgsfoeok"
  array[2] = "bsqdunldltkxripsaxkaojfiovnxulshiohbumirjzgfmovowiv"
  array[3] = "hepdatupehcbbdyhyjsnvtzhfupd"
  array[4] = "rlrezqqlfpoktoiitbtyngyyyeoglmwxfrxjtejmyy"
  sort(array)
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
