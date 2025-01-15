import java.io.File

const val DIST = 501

enum class Dir(val delRow: Int, val delCol: Int) {
  UP(-1, 0),
  DOWN(1, 0),
  LEFT(0, -1),
  RIGHT(0, 1),
}

fun main() {
  val bufferedReader = File("input.txt").bufferedReader()
  val inputString = bufferedReader.use { it.readText() }.trim()

  val grid = inputString.split('\n').map { it.toCharArray().toTypedArray() }.toTypedArray()

  val m = grid.size
  val n = grid.first().size

  val sRow = grid.indexOfFirst { it.contains('S') }
  val sCol = grid[sRow].indexOf('S')

  grid[sRow][sCol] = '.'

  fun isValid(row: Int, col: Int): Boolean {
    return 0 <= row && row < m && 0 <= col && col < n
  }

  fun Int.normRow(): Int {
    return (this % m + m) % m
  }

  fun Int.normCol(): Int {
    return (this % n + n) % n
  }

  var visited = mutableSetOf<Pair<Int, Int>>(Pair(sRow, sCol))

  for (rep in 1..DIST) {
    val newVisited = mutableSetOf<Pair<Int, Int>>()

    for ((row, col) in visited) {
      for (dir in Dir.entries) {
        val newRow = row + dir.delRow
        val newCol = col + dir.delCol
        if (isValid(newRow, newCol) && grid[newRow.normRow()][newCol.normCol()] == '.') {
          newVisited.add(Pair(newRow, newCol))
        }
      }
    }

    visited = newVisited
  }

  println(visited.size)
}
