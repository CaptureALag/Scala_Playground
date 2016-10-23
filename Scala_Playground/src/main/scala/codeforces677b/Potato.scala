package codeforces677b

class Potato(
              val id: Int,
              val height: Int,
              val heightLeft: Int
            ) {

  require(height > 0)
  require(heightLeft > 0)

  def slice(howMuch: Int): Potato =
    new Potato(id, height, heightLeft - howMuch)
}