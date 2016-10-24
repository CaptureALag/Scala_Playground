import scalaz._, Scalaz._

type StringLogger[T] = Writer[DList[String], T]

type StackState[T] = StateT[StringLogger, List[Int], T]

def f : StackState[Int] = {
  StateT[StringLogger, List[Int], Int] (s =>
    Writer[DList[String], (List[Int], Int)] (
      DList("Prepending 1"),
      (1::s, (1::s).sum)
    )
  )
}

def g : StackState[Int] = {
  StateT[StringLogger, List[Int], Int] (s =>
    Writer[DList[String], (List[Int], Int)] (
      DList("Appending 99"),
      (s :+ 99, 99)
    )
  )
}

def h : StackState[Int] = {
  StateT[StringLogger, List[Int], Int] (s =>
    Writer[DList[String], (List[Int], Int)] (
      DList("Prepending 50"),
      (50::s, 50)
    )
  )
}

Stream.continually(()).scanLeft(
  Writer[DList[String], (List[Int], Int)] (
    DList(),
    (List(),10)
  )
) ( (accum, _) =>
  accum.flatMap({
    case (lst, _) =>
      f.run(lst)
  })
).find(_.value._2 > 10).get