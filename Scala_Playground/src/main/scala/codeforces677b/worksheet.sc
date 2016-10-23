import scalaz._, Scalaz._

type ListWriter[T] = Writer[DList[String], T]

def f = Writer[DList[String], String] (
  DList("123"),
  "abc"
)

def g = Writer[DList[String], String] (
  DList("456"),
  "def"
)

def h = Writer[DList[String], String] (
  DList("789"),
  "ghi"
)

List(
  f,
  g,
  h
).sequence[ListWriter, String].run._1.toList

f.run._1.formatted("")