let fn: (Int...) -> () = {
  let _: [Int] = $0
}

let fna: (Int...) -> () = { (x: Int...) in
  let _: [Int] = x
}

let fn2: (Int, Int...) -> () = {
  let _: Int = $0
  let _: [Int] = $1
}

let fna2: (Int, Int...) -> () = {
  (x: Int, y: Int...) in
}

let fn3: (Int, Int...) -> () = {
  let _: Int = $0.0
  let _: [Int] = $0.1
}

let fna3: (Int, Int...) -> () = {
  (x: Int, y: Int...) in
}

func g<T>(t: T) {
  let gfn: (T...) -> () = {
    let _: [T] = $0
  }

  let gfna: (T...) -> () = { (x: T...) in
    let _: [T] = x
  }

  let gfn2: (T, T...) -> () = {
    let _: T = $0
    let _: [T] = $1
  }

  let gfna2: (T, T...) -> () = {
    (x: T, y: T...) in
  }

  let gfn3: (T, T...) -> () = {
    let _: T = $0.0
    let _: [T] = $0.1
  }

  let gfna3: (T, T...) -> () = {
    (x: T, y: T...) in
  }
}

func gg<T>(t: T) {
  let gfn: (inout T) -> () = {
    let _: T = $0
  }

  let gfn2: (T, inout T) -> () = {
    let _: T = $0
    let _: T = $1
  }

  /*let gfn3: (T, inout T) -> () = {
    let _: T = $0.0
    let _: T = $0.1
  }*/
}


func fff<T>(fn: @escaping (T...) -> ()) -> ([T]) -> () {
  return fn
}

func fff<T>(fn: @escaping ([T]) -> ()) -> (T...) -> () {
  return fn
}

func ggg(_: Int...) {}

let ffn: ([Int]) -> () = ggg

ffn([1, 2, 3])
